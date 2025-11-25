module HerbalizerSpec (spec) where

import Test.Hspec
import System.Process
import System.Exit

-- Helper to run herbalizer with input
runHerbalizer :: String -> IO String
runHerbalizer input = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "herbalizer" [] input
  case exitCode of
    ExitSuccess -> return stdout
    ExitFailure _ -> return $ "ERROR: " ++ stderr

spec :: Spec
spec = do
  describe "Basic Tag Conversion" $ do
    it "converts simple div" $ do
      result <- runHerbalizer "%div\n"
      result `shouldBe` "<div></div>\n"

    it "converts div with class" $ do
      result <- runHerbalizer "%div.test\n"
      result `shouldBe` "<div class=\"test\"></div>\n"

    it "converts div with id" $ do
      result <- runHerbalizer "%div#main\n"
      result `shouldBe` "<div id=\"main\"></div>\n"

    it "converts div with multiple classes" $ do
      result <- runHerbalizer "%div.foo.bar\n"
      result `shouldBe` "<div class=\"foo bar\"></div>\n"

    it "converts div with plain text" $ do
      result <- runHerbalizer "%div Hello\n"
      result `shouldBe` "<div>Hello</div>\n"

  describe "Ruby Interpolation" $ do
    it "converts Ruby string interpolation to ERB" $ do
      result <- runHerbalizer "%h1 Hello #{user.name}\n"
      result `shouldBe` "<h1>Hello <%= user.name %></h1>\n"

    it "handles multiple interpolations" $ do
      result <- runHerbalizer "%p #{first} and #{second}\n"
      result `shouldBe` "<p><%= first %> and <%= second %></p>\n"

    it "handles nested braces in interpolation" $ do
      result <- runHerbalizer "%p #{items.map {|x| x.value}}\n"
      result `shouldBe` "<p><%= items.map {|x| x.value} %></p>\n"

  describe "Tailwind CSS Classes" $ do
    it "handles colons in class names" $ do
      result <- runHerbalizer "%div.md:flex\n"
      result `shouldBe` "<div class=\"md:flex\"></div>\n"

    it "handles brackets in class names" $ do
      result <- runHerbalizer "%div.w-[200px]\n"
      result `shouldBe` "<div class=\"w-[200px]\"></div>\n"

    it "handles slashes in class names" $ do
      result <- runHerbalizer "%div.w-1/2\n"
      result `shouldBe` "<div class=\"w-1/2\"></div>\n"

    it "handles complex Tailwind classes" $ do
      result <- runHerbalizer "%div.md:flex.lg:hidden.w-full\n"
      result `shouldBe` "<div class=\"md:flex lg:hidden w-full\"></div>\n"

    it "handles adjacent breakpoint classes" $ do
      result <- runHerbalizer "%div.md:grid-cols-2.3xl:grid-cols-3\n"
      result `shouldBe` "<div class=\"md:grid-cols-2 3xl:grid-cols-3\"></div>\n"

    it "handles fractional classes via hash syntax" $ do
      result <- runHerbalizer "%div{ class: \"py-0.5\" }\n"
      result `shouldBe` "<div class=\"py-0.5\"></div>\n"

  describe "Nested Hash Attributes" $ do
    it "expands data hash to data attributes" $ do
      result <- runHerbalizer "%div{ data: {controller: \"test\"} }\n"
      result `shouldBe` "<div data-controller=\"test\"></div>\n"

    it "expands multiple data attributes" $ do
      result <- runHerbalizer "%div{ data: {controller: \"menu\", action: \"click\"} }\n"
      result `shouldBe` "<div data-controller=\"menu\" data-action=\"click\"></div>\n"

    it "handles quoted keys with hyphens" $ do
      result <- runHerbalizer "%div{ data: {\"custom-key\": \"value\"} }\n"
      result `shouldBe` "<div data-custom-key=\"value\"></div>\n"

    it "handles method calls with arguments in nested hash values" $ do
      result <- runHerbalizer "%div{ data: {controller: \"test\", url: path(a, b, c)} }\n"
      result `shouldContain` "data-controller=\"test\""
      result `shouldContain` "data-url=\"path(a, b, c)\""

    it "converts Ruby symbols to strings in nested hash values" $ do
      result <- runHerbalizer "%div{ data: {method: :put, action: :delete} }\n"
      result `shouldContain` "data-method=\"put\""
      result `shouldContain` "data-action=\"delete\""

  describe "Conditional Attributes" $ do
    it "renders conditional attributes with ternary ending in nil" $ do
      result <- runHerbalizer "%option{ selected: (x == y) ? \"selected\" : nil }\n"
      result `shouldContain` "<%= ' selected=\"selected\"' if"

    it "handles value attributes with expressions" $ do
      result <- runHerbalizer "%img{ src: user.avatar }\n"
      result `shouldContain` "src=\"<%= user.avatar %>\""

  describe "Ruby Blocks" $ do
    it "converts if block" $ do
      result <- runHerbalizer "- if condition\n  %div Content\n"
      result `shouldContain` "<% if condition %>"

    it "converts elsif block" $ do
      result <- runHerbalizer "- elsif other\n  %div Other\n"
      result `shouldContain` "<% elsif other %>"

  describe "Quoted String Keys" $ do
    it "handles double-quoted keys with colon syntax" $ do
      result <- runHerbalizer "%div{ \"aria-label\": \"test\" }\n"
      result `shouldBe` "<div aria-label=\"test\"></div>\n"

  describe "Edge Cases" $ do
    it "handles files without trailing newline" $ do
      result <- runHerbalizer "%div Test"
      result `shouldBe` "<div>Test</div>\n"

    it "handles blank lines at start" $ do
      result <- runHerbalizer "\n%div Test\n"
      result `shouldBe` "<div>Test</div>\n"

    it "handles blank lines between content" $ do
      result <- runHerbalizer "%div First\n\n%div Second\n"
      result `shouldBe` "<div>First</div>\n<div>Second</div>\n"

  describe "Self-closing Tags" $ do
    it "handles img tags" $ do
      result <- runHerbalizer "%img\n"
      result `shouldBe` "<img/>\n"

    it "handles br tags" $ do
      result <- runHerbalizer "%br\n"
      result `shouldBe` "<br/>\n"

  describe "Ruby Expressions" $ do
    it "converts = expression to ERB output" $ do
      result <- runHerbalizer "= user.name\n"
      result `shouldContain` "<%= user.name %>"

    it "converts - expression to ERB execution" $ do
      result <- runHerbalizer "- x = 5\n"
      result `shouldContain` "<% x = 5 %>"

  describe "Plain Text with Interpolation" $ do
    it "handles plain text starting with Ruby interpolation" $ do
      result <- runHerbalizer "%small\n  #{user.name}\n"
      result `shouldContain` "<%= user.name %>"

    it "handles plain text with multiple interpolations" $ do
      result <- runHerbalizer "%p\n  #{first} and #{second}\n"
      result `shouldContain` "<%= first %> and <%= second %>"

  describe "Complex Real-world Examples" $ do
    it "handles Stimulus data attributes" $ do
      result <- runHerbalizer "%div{ data: {controller: \"toggle\", action: \"click->toggle#fire\"} }\n"
      result `shouldContain` "data-controller=\"toggle\""
      result `shouldContain` "data-action=\"click->toggle#fire\""
