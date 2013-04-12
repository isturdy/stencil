{-# LANGUAGE OverloadedStrings #-}

module Text.StencilTest (stencilTests) where

import           Test.Framework                 (testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit

import qualified Data.Map                       as Map
import           Data.Text

import           Text.Stencil

stencilTests = testGroup "Text/Stencil" [
    parserTests
  , valueTests
  , outputTests
  ]

parserTests = testGroup "parser" [

    testCase "delimiter mismatch ('<')" $
    isParseError " <<<()>>"

  , testCase "delimiter mismatch ('>')" $
    isParseError " <<()>>>"

  , testCase "delimiter mismatch (')')" $
    isParseError " <<(()>>)"

  , testCase "delimiter mismatch (')')" $
    isParseError " <<())>>)"

  , testCase "incorrect arity" $
    isParseError " <<(?name)>>"

  , testCase "incorrect arity 2" $
    isParseError " <<(?name|text)>>"

  , testCase "Escaping" $
    testTemp " ||| <<<( )>>> <<<<(( ))>>>>" " | <( )> <<(( ))>>"

  ]

valueTests = testGroup "value conversion" [

    testCase "Text" $
    testTemp " <<(text)>> " " textvalue "

  , testCase "String" $
    testTemp " <<(string)>> " " stringvalue "

  , testCase "Dictionary" $
    testTemp " <<(%dict|<<(dictsub)>>)>> " " dictsubvalue "

  , testCase "Map" $
    testTemp " <<(%map|<<(dictsub)>>)>> " " dictsubvalue "

  , testCase "Association list" $
    testTemp " <<(%alist|<<(dictsub)>>)>> " " dictsubvalue "

  , testCase "Dictionary list" $
    testTemp "<<(@dlist|(<<(dictsub)>>)|)>>" "(dictsubvalue1)(dictsubvalue2)"

  , testCase "Text list" $
    testTemp " <<(@tlist|(<<()>>)|)>> " " (textsubvalue1)(textsubvalue2) "

  , testCase "Haskell value (default)" $
    testTemp " <<(hvaldef)>> " " (def 1) "

  , testCase "Haskell value (no default)" $
    testTempWarn " <<(hval)>> " "  "
    "'hval' refers to a value of the wrong type. Expecting text."

  , testCase "Haskell function" $
    testTemp " <<($hfun|hval)>> " " 2 "

  , testCase "Text function" $
    testTemp " <<(!tfun|arst)>> " " ARST "

  , testCase "Showable function" $
    testTemp " <<(showable)>> " " 2 "

  ]

outputTests = testGroup "output" [

    testCase "substitution (undefined)" $
    testTempWarn  " (<<(undef)>>) " " () " "name 'undef' not in dictionary."

  , testCase "if (defined)" $
    testTemp " <<(?text|primary|alternate)>> " " primary "

  , testCase "if (undefined)" $
    testTemp " <<(?undef|primary|alternate)>> " " alternate "

  , testCase "list (undefined)" $
    testTempWarn " (<<(@undef||)>>) " " () " "name 'undef' not in dictionary."

  , testCase "list (empty)" $
    testTemp " <<(@nillist|primary|alternate)>> " " alternate "

  , testCase "template (undefined)" $
    testTempWarn " (<<(&undef)>>) " " () " "Template 'undef' not found."

  , testCase "template (defined)" $
    testTemp " (<<(&temp)>>) " " ( textvalue ) "

  , testCase "HTML escaping" $
    testTemp " <<((html))>> " " &lt;&gt; "

  ]

defTemplates :: Templates
defTemplates = Map.fromList [("temp", " <<(text)>> ")]

defContext :: Context
defContext = return . toDict $ [
    ("text"::Text, toValue ("textvalue"::Text))
  , ("string", toValue ("stringvalue"::String))
  , ("dict", toValue $ toDict
                       [("dictsub"::Text, textValue "dictsubvalue")])
  , ("map", toValue . Map.fromList$
                      [("dictsub"::Text, textValue "dictsubvalue")])
  , ("alist", toValue [("dictsub"::Text, textValue"dictsubvalue")])
  , ("dlist", toValue . fmap toDict $
                    [[("dictsub"::Text, textValue "dictsubvalue1")]
                    , [("dictsub"::Text, textValue "dictsubvalue2")]])
  , ("tlist", toValue ["textsubvalue1"::Text, "textsubvalue2"])
  , ("hval", hValue (1::Int))
  , ("hvaldef", hValueWithDefault "(def 1)" (1::Int))
  , ("hfun", toValue (pack . show . (+(1::Int))))
  , ("tfun", toValue toUpper)
  , ("showable", toValue (2::Int))
  , ("nillist", toValue ([]::[Text]))
  , ("nildlist", toValue ([]::[Dictionary]))
  , ("html", textValue "<>")
  ]

runTemp :: Template -> Either Text (Text, Warnings)
runTemp = evalTemplate defTemplates defContext

testTemp :: Template -> Text -> Assertion
testTemp temp res = runTemp temp @?= Right (res, [])

testTempWarn :: Template -> Text -> Text -> Assertion
testTempWarn temp res warn = runTemp temp @?= Right (res, [warn])

isParseError :: Template -> Assertion
isParseError t = snipLeft (runTemp t) @?= Left ""

snipLeft :: Either Text a -> Either Text a
snipLeft (Left t) = Left ""
snipLeft x        = x
