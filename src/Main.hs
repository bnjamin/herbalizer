{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import Control.Applicative ((<$>), (<*>), (<*), (<$))
import Control.Monad (liftM)
import Control.Monad.State (State, runState)
import Control.Applicative ((<*))
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Text.Parsec.Pos
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, intercalate, intersperse)
import qualified Data.Map as M
import Text.Regex.Posix
import System.Environment

type IParser a = IndentParser String () a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse p s inp = runIndent $ runParserT p () s inp

data Tree = Tree Expression [Tree]
    deriving (Show)

type Attrs = [(String, String)]
data InlineContent = RubyInlineContent String 
    | PlainInlineContent String
    | NullInlineContent
    | HamlFilterContent String
    deriving (Show, Eq)


type IsFormTag = Bool

data Expression = 
      DocType String
    | Comment String 
    | PlainText String
    | RubyStartBlock String IsFormTag
    | RubyMidBlock String
    | RubySideEffect String
    | RubyExp String
    | Tag String Attrs InlineContent
    | GenericExpression String 
    deriving (Show)

container :: IParser Tree
container = do
  b <- withBlock Tree expression container
  spaces
  return b

expression :: IParser Expression
expression = (try escapeHtmlExpr <|> docType) <|> comment <|> hamlFilter <|> startPlainText <|> rubyBlock <|> rubyExp <|> tag <|>  genericExpression
  
rubyBlock = do
    char '-'
    spaces
    k <- rubyKeyword
    firstLine <- manyTill anyChar newline <* spaces
    continuationLines <- if needsContinuation firstLine
                         then option [] (try indentedOrBlank)
                         else return []
    let cleanedContinuation = intercalate "\n" $ filter (not . null) $ map (dropWhile (`elem` " \t")) $ lines $ concat continuationLines
        fullLine = if null continuationLines
                   then k ++ firstLine
                   else k ++ firstLine ++ "\n" ++ cleanedContinuation
    if (k `elem` midBlockKeywords)
    then return (RubyMidBlock fullLine)
    -- TODO : we need to recognize Ruby expression expression included purely for a side effect,
    -- e.g. "- localvar = Time.now"
    else return (RubyStartBlock fullLine False)
  where
    midBlockKeywords = ["else", "elsif", "rescue", "ensure", "when", "end"]
    needsContinuation line =
      let trimmed = reverse $ dropWhile (`elem` " \t") $ reverse line
      in not (null trimmed) && (last trimmed == ',' || hasUnbalancedDelimiters trimmed)
    hasUnbalancedDelimiters s =
      let opens = length (filter (`elem` "([{") s)
          closes = length (filter (`elem` ")]}") s)
      in opens /= closes

escapeHtmlExpr = do
  char '!'
  line <- ((:) <$> char '=' >> spaces >> manyTill anyChar newline <* spaces)
  return $ RubyExp $ "raw(" ++ line ++ ")"

rubyExp = do
  firstLine <- ((:) <$> char '=' >> spaces >> manyTill anyChar newline <* spaces)
  continuationLines <- if needsContinuation firstLine
                       then option [] (try indentedOrBlank)
                       else return []
  let cleanedContinuation = intercalate "\n" $ filter (not . null) $ map (dropWhile (`elem` " \t")) $ lines $ concat continuationLines
      fullExpression = if null continuationLines
                       then firstLine
                       else firstLine ++ "\n" ++ cleanedContinuation
  return (RubyExp fullExpression)
  where
    needsContinuation line =
      let trimmed = reverse $ dropWhile (`elem` " \t") $ reverse line
      in not (null trimmed) && (last trimmed == ',' || hasUnbalancedDelimiters trimmed)
    hasUnbalancedDelimiters s =
      let opens = length (filter (`elem` "([{") s)
          closes = length (filter (`elem` ")]}") s)
      in opens /= closes

tag :: IParser Expression
tag = do
    tag <- explicitTag <|> return "div"
    as <- many (dotClass <|> idHash)
    hs <- option [] (hashAttrs)
    many $ oneOf " \t"
    c <- parseInlineContent 
    spaces
    return $ Tag tag (attrs as hs) c
  where 
    attrs as hs = filter (\(k, v) -> v /= "") $ 
      M.toList $ 
      M.unionWith (\a b -> intercalate " " (filter (/= "") [a,b]))
        (M.fromList hs)
        (M.fromList (makeClassIdAttrs as)) 
    parseInlineContent = (RubyInlineContent <$> (char '=' >> spaces >> manyTill anyChar newline)) <|> 
        (PlainInlineContent <$> (manyTill anyChar newline)) 
        <|> return NullInlineContent

makeClassIdAttrs :: [String] -> [(String, String)]
makeClassIdAttrs cs = classes : [("id", ids)]
    where classes = ("class", intercalate " " $ map tail $ filter ("." `isPrefixOf`) cs )
          ids = intercalate " " $ map tail $ filter (isPrefixOf "#") cs


explicitTag = do
  char '%'
  tag <- many alphaNum
  return tag

dotClass = (:) <$> char '.' <*> cssClassOrId
idHash = (:) <$> char '#' <*> cssClassOrId

hashAttrs = do
  char '{' 
  xs <- kvPair `sepBy` (spaces >> char ',' >> spaces)
  char '}'
  return xs

cssClassOrId = do
  first <- many (alphaNum <|> oneOf "-_:[]/")
  rest <- option "" (try $ do
    char '.'
    d <- digit
    remainder <- many (alphaNum <|> oneOf "-_:[]/")
    return $ "." ++ [d] ++ remainder)
  return $ first ++ rest
rubyIdentifier = many (alphaNum <|> char '_')

rubyKeyword = many alphaNum

singleQuotedStr = do
    between (char '\'') (char '\'') (many stringChar)
  where stringChar = ('\'' <$ string "\\'") <|> (noneOf "'")

doubleQuotedStr = do
    between (char '"') (char '"') (many stringChar)
  where stringChar = ('"' <$ string "\\\"") <|> (noneOf "\"")

--- Ruby interpolation delimiters crudley replaced by ERB style
rubyString = do
    between (char '"') (char '"') rString
  where 
    rString = liftM replaceInterpolationDelim $ many stringChar
    stringChar = ('"' <$ string "\\\"") <|> (noneOf "\"") 
    replaceInterpolationDelim = (replace "#{" "<%= ") . (replace "}" " %>")

rubySymbol =  do
      char ':' 
      xs <- (char '"' >> many stringChar2 <* char '"') <|> (char '\'' >> many stringChar1 <* char '\'') <|> rubyIdentifier 
      return xs
  where stringChar1 = ('\'' <$ string "\\'") <|> (noneOf "'")
        stringChar2 = ('"' <$ string "\\\"") <|> (noneOf "\"")

rubySymbolKey = rubyIdentifier <* char ':'

-- really, we need to parse full-blown Ruby expressions
rubyValue = do
    expr <- parseRubyExpr
    return $ "<%= " ++ expr ++ " %>"
  where
    parseRubyExpr = do
      parts <- many1 parseRubyPart
      return $ concat parts
    parseRubyPart =
      (try $ betweenStuff '(' ')') <|>
      (try $ betweenStuff '[' ']') <|>
      (try $ betweenStuff '{' '}') <|>
      (try $ do
        s <- many1 (noneOf "},()[]")
        spaces
        return $ s ++ " ")
    betweenStuff x y = do
      char x
      xs' <- nestedContent y
      char y
      spaces
      return $ [x] ++ xs' ++ [y] ++ " "
    nestedContent closingChar = do
      concat <$> many (
        (try $ betweenStuff '{' '}') <|>
        (try $ betweenStuff '[' ']') <|>
        (try $ betweenStuff '(' ')') <|>
        (try $ do
          c <- noneOf [closingChar]
          return [c])
        )

rocket = spaces >> string "=>" >> spaces
aKey = (try $ singleQuotedStr <* rocket)
  <|> (try $ doubleQuotedStr <* rocket)
  <|> (try $ singleQuotedStr <* char ':' <* spaces)
  <|> (try $ doubleQuotedStr <* char ':' <* spaces)
  <|> (try $ rubySymbol <* rocket)
  <|> (rubySymbolKey <* spaces)

aValue = try nestedHash <|> singleQuotedStr <|> rubyString <|> many1 digit <|> rubyValue
  where
    nestedHash = do
      char '{'
      content <- nestedHashContent
      char '}'
      return $ "{" ++ content ++ "}"
    nestedHashContent = concat <$> many (
      (try $ do
        char '{'
        inner <- nestedHashContent
        char '}'
        return $ "{" ++ inner ++ "}") <|>
      (try $ do
        char '"'
        s <- many (noneOf "\"")
        char '"'
        return $ "\"" ++ s ++ "\"") <|>
      (try $ do
        char '\''
        s <- many (noneOf "'")
        char '\''
        return $ "'" ++ s ++ "'") <|>
      (do
        c <- noneOf "{}"
        return [c])
      )

kvPair :: IParser (String, String)
kvPair = do
  k <- (many $ oneOf " \t") >> aKey 
  v <- spaces >> aValue <* (many $ oneOf " \t")
  return (k, v)

-- TODO HTML Comments are not rendered like HAML renders them
-- also HAML -# style comments could be rendered
comment :: IParser Expression
comment = do
  char '/' 
  s <- manyTill anyChar newline
  spaces
  return $ Comment s

docType :: IParser Expression
docType = do
    string "!!!"
    many $ char ' '
    s <- option [] $ many alphaNum
    newline
    return $ DocType s

filterBlock p = withPos $ do
    r <- many (checkIndent >> p)
    return r

hamlFilter = do
  withPos $ do
    char ':' 
    s <- many $ alphaNum
    many (oneOf " \t")
    newline
    xs <- indentedOrBlank
    return $ Tag (convertToTag s) [] (HamlFilterContent $ concat xs)
  where convertToTag "javascript" = "script"
        convertToTag s = s

indentedOrBlank = many1 (try blankLine <|> try indentedLine)

indentedLine :: IParser String
indentedLine = do
    a <- many $ oneOf " \t"
    indented
    xs <- manyTill anyChar newline 
    return $ a ++ xs ++ "\n"

blankLine = do
    a <- many $ oneOf " \t"
    newline 
    return $ a ++ "\n"

-- TODO; check how this deals with plain text that actually starts with these characters
-- Not sure what HAML's escaping rules are here; again HAML makes things unclear & make you
-- to look at docs

startPlainText = do
  spaces
  -- Allow # if followed by { (Ruby interpolation), otherwise exclude it
  first <- (try $ string "#{" >> return "#{") <|> (noneOf "-=.#%" >>= \c -> return [c])
  rest <- manyTill anyChar newline
  spaces
  return $ PlainText (first ++ rest)

genericExpression :: IParser Expression
genericExpression = do
  spaces
  s <- manyTill anyChar newline
  spaces
  return $ GenericExpression s


------------------------------------------------------------------------
-- output ERB
-- turn tree structure into an array of lines, including closing tags and indentation level


type Nesting = Int

-- This is the main processing entrypoint
processChildren :: Nesting -> [Tree] -> [String]
processChildren n xs = concat $ map (erb n) $ (rubyEnd xs)

erb ::  Nesting -> Tree -> [String]

erb n tree@(Tree (Tag t a i) []) 
    | t `elem` selfClosingTags = [pad n ++ selfClosingTag tree]
    -- basically ignores inline content
  where selfClosingTags = ["br", "img", "hr", "meta"]

-- no children; no padding, just tack closing tag on end
erb n tree@(Tree (Tag t a i) []) = [pad n ++ startTag tree ++ endTag 0 tree] 

erb n tree@(Tree (Tag t a i) xs) = (pad n ++ startTag tree) : ((processChildren (n + 1) xs) ++ [endTag n tree])

erb n tree@(Tree (RubyStartBlock s isform) xs) = 
    (pad n ++ (starttag isform) ++ s ++ " %>") : (processChildren (n + 1) xs)
  where 
    starttag True = "<%= "
    starttag False = "<% "


erb n tree@(Tree (RubyMidBlock s) xs) = 
    (pad n ++ "<% " ++ s ++ " %>") : (processChildren (n + 1) xs)

erb n tree@(Tree (RubyExp s) _) = [pad n ++ "<%= " ++ s ++ " %>"] 

erb n tree@(Tree (RubySideEffect s) []) = [pad n ++ "<% " ++ s ++ " %>"] 

erb n tree@(Tree (PlainText s) _) = [pad n ++ convertInterpolations s] 
erb n tree@(Tree (Comment s) xs) = (pad n ++ "<!--" ++ s) : ((processChildren (n + 1) xs) ++ [pad n  ++ "-->"])

-- DocTypes
erb n tree@(Tree (DocType s) _) = [d s]
  where
    d "" = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
    d "Strict" = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
    d "Frameset" = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">"
    d "1.1" = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
    d "Basic" = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Basic 1.1//EN\" \"http://www.w3.org/TR/xhtml-basic/xhtml-basic11.dtd\">"
    d "Mobile" = "<!DOCTYPE html PUBLIC \"-//WAPFORUM//DTD XHTML Mobile 1.2//EN\" \"http://www.openmobilealliance.org/tech/DTD/xhtml-mobile12.dtd\">"
    d "RDFa" = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML+RDFa 1.0//EN\" \"http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd\">"
    d "5" = "<!DOCTYPE html>"


    d _ = "TEST"

erb n x@_ = [pad n ++ show x]


-- Ruby expressions with no output and no children; convert to RubySideEffect type
rubyEnd ((Tree (RubyStartBlock s False) []):xs) = rubyEnd $ (Tree (RubySideEffect s) []):xs 

-- Try to insert "<% end %>" tags correctly
rubyEnd (x@(Tree (RubyStartBlock _ _) _):y@(Tree (RubyMidBlock _) _):xs) = 
    x:(rubyEnd (y:xs))  -- just shift the cursor to the right
rubyEnd (x@(Tree (RubyMidBlock _) _):y@(Tree (RubyMidBlock _) _):xs) = 
    x:(rubyEnd (y:xs))
rubyEnd (x@(Tree (RubyStartBlock _ _) _):xs) = x:endTagTree:(rubyEnd xs)
rubyEnd (x@(Tree (RubyMidBlock _) _):xs) = x:endTagTree:(rubyEnd xs)

-- RubyExp with children is probably a form_for or the like; convert to a RubyStartBlock
rubyEnd (x@(Tree (RubyExp s) children@(c:cs)):xs) = rubyEnd $ (Tree (RubyStartBlock s True) children):xs

-- Move inline Ruby expressions to child tree
rubyEnd (x@(Tree (Tag t a (RubyInlineContent s)) ts):xs) = 
  (Tree (Tag t a NullInlineContent) ((Tree (RubyExp s) []):ts)):(rubyEnd xs)

-- erb content should pass through
rubyEnd (x@(Tree (Tag "erb" a (HamlFilterContent s)) ts):xs) = (Tree (PlainText s) []):(rubyEnd xs)

-- Move HamlFilterContent to child tree
rubyEnd (x@(Tree (Tag t a (HamlFilterContent s)) ts):xs) = (Tree (Tag t a NullInlineContent) ((Tree (PlainText ('\n':s)) []):ts)):(rubyEnd xs)

rubyEnd (x:xs) = x : (rubyEnd xs)
rubyEnd [] = []


endTagTree = Tree (PlainText "<% end %>") []


startTag :: Tree -> String
startTag (Tree (Tag t a i) _) = "<" ++ t ++ showAttrs a ++ ">" ++ showInlineContent i

endTag :: Int -> Tree -> String
endTag n (Tree (Tag t _ _) _) = pad n ++ "</" ++ t ++ ">"

selfClosingTag :: Tree -> String
selfClosingTag (Tree (Tag t a _) _) = "<" ++ t ++ showAttrs a ++ "/>"

showAttrs xs = case concatMap expandAttr xs of
      [] -> ""
      xs' -> " " ++ intercalate " " xs'
    where
      makeAttr (k,v)
        | take 3 v == "<%=" && take 1 (reverse v) == ">" =
            let expr = trim $ drop 3 $ take (length v - 3) v
            in if isConditionalExpr expr
               then "<%= ' " ++ k ++ "=\"" ++ k ++ "\"' if (" ++ expr ++ ") %>"
               else k ++ "=\"<%= " ++ expr ++ " %>\""
        | otherwise = intercalate "=" [k, "\"" ++ v ++ "\"" ]
      isConditionalExpr s =
        let trimmed = trim s
        in (": nil" `isSuffixOf` trimmed) ||
           (": false" `isSuffixOf` trimmed) ||
           ("? nil :" `isInfixOf` trimmed) ||
           ("? false :" `isInfixOf` trimmed) ||
           ("?nil:" `isInfixOf` (filter (/= ' ') trimmed)) ||
           ("?false:" `isInfixOf` (filter (/= ' ') trimmed))
      expandAttr (k,v)
        | (k == "data" || k == "aria") && isNestedHash v = expandNestedHash k v
        | otherwise = [makeAttr (k,v)]
      trim = dropWhile (`elem` " \t") . reverse . dropWhile (`elem` " \t") . reverse
      isNestedHash s = take 1 s == "{" && take 1 (reverse s) == "}"
      expandNestedHash prefix hash =
        let content = take (length hash - 2) (drop 1 hash)
            pairs = parseHashPairs content
        in map (\(k,v) -> makeAttr (prefix ++ "-" ++ k, stripQuotes v)) pairs
      parseHashPairs s = parsePairs s []
        where
          parsePairs [] acc = reverse acc
          parsePairs str acc =
            let (pair, rest) = breakPair str
            in case pair of
              Just p -> parsePairs rest (p:acc)
              Nothing -> reverse acc
          breakPair str =
            let trimmed = dropWhile (`elem` " \t,") str
                (key, afterKey) = extractKey trimmed
                afterColon = dropWhile (`elem` " \t") (dropWhile (== ':') afterKey)
                (value, afterValue) = extractValue afterColon
            in if null key then (Nothing, "") else (Just (key, value), afterValue)
          extractKey ('"':rest) =
            let (k, afterQuote) = span (/= '"') rest
            in (k, if null afterQuote then "" else tail afterQuote)
          extractKey str = span (\c -> c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['_'])) str
          extractValue ('"':rest) =
            let (val, afterQuote) = span (/= '"') rest
            in (val, if null afterQuote then "" else tail afterQuote)
          extractValue str = span (\c -> c `notElem` " \t,}") str
      stripQuotes s = s

showInlineContent (PlainInlineContent s) = convertInterpolations s
showInlineContent (NullInlineContent) = ""
-- should not be reached:
showInlineContent (RubyInlineContent s) = "RUBY: " ++ s

showInlineContent s = "\nERROR: No showInlineContent for " ++ (show s) ++ "\n"

-- Convert Ruby string interpolations #{...} to ERB <%= ... %>
convertInterpolations :: String -> String
convertInterpolations [] = []
convertInterpolations str@('#':'{':rest) =
  case extractInterpolation rest of
    Just (expr, remaining) -> "<%= " ++ expr ++ " %>" ++ convertInterpolations remaining
    Nothing -> '#':'{': convertInterpolations rest
convertInterpolations (c:cs) = c : convertInterpolations cs

extractInterpolation :: String -> Maybe (String, String)
extractInterpolation str = extract str 0 []
  where
    extract [] _ _ = Nothing
    extract ('}':rest) 0 acc = Just (reverse acc, rest)
    extract ('{':rest) depth acc = extract rest (depth + 1) ('{':acc)
    extract ('}':rest) depth acc = extract rest (depth - 1) ('}':acc)
    extract (c:rest) depth acc = extract rest depth (c:acc)

    
pad :: Int -> String
pad n = take (n * 2) $ repeat ' ' 

------------------------------------------------------------------------

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
    Left err -> Left err
    Right ys -> case f x of 
                  Left err -> Left err
                  Right y -> Right (y:ys)
mapEithers _ _ = Right []


-- the following functions are extracted from MissingH Data.List.Utils 
-- by John Goerzen <jgoerzen@complete.org>

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = join new . split old $ l

join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])

spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs

split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (startswith delim) str
        in
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else split delim
                                                 (drop (length delim) x)


startswith :: Eq a => [a] -> [a] -> Bool
startswith = isPrefixOf


------------------------------------------------------------------------

parse1 s = iParse container "" s 

-- http://stackoverflow.com/questions/15549050/haskell-parsec-how-do-you-use-the-functions-in-text-parsec-indent
parseIndent :: IndentParser String () a -> String -> Either ParseError a
parseIndent p src = runIndent $ runParserT p () "" src

topLevelsParser1 = many1 (topLevelItem)

topLevelItem = do
  withPos $ do
    as <- manyTill anyChar newline
    xs <- option [] (try indentedOrBlank)
    return $ as ++ "\n" ++ concat xs

parseTopLevels s =
    let s' = if null s || last s /= '\n' then s ++ "\n" else s
    in case (parseIndent topLevelsParser1 s') of
      Left err -> putStrLn (show err)
      Right chunks -> do
        let nonBlankChunks = filter (not . isBlankChunk) chunks
        case (mapEithers parse1 nonBlankChunks) of
          Left err -> putStrLn . show $ err
          Right trees -> do
            mapM_ putStrLn $ processChildren 0 trees
  where
    isBlankChunk s = all (`elem` " \t\n") s

main = do
    args <- getArgs
    if (null args)
    then
      getContents >>= parseTopLevels 
    else
      mapM_ (\f -> readFile f >>= parseTopLevels) args
      
