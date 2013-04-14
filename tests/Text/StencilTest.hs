{-# LANGUAGE OverloadedStrings #-}

module Text.StencilTest (stencilTests) where

import           Test.Framework                 (testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit

import qualified Data.Map                       as Map
import           Data.Text

import           Text.Stencil
import           Text.Stencil.Internal

stencilTests = testGroup "Text/Stencil" [
    parserTests
  , valueTests
  , outputTests
  ]

parserTests = testGroup "parser" [

    testCase "delimiter mismatch ('<')" $
    isParseError " <()>>"

  , testCase "delimiter mismatch ('>')" $
    isParseError " <<()>"

  , testCase "delimiter mismatch (')')" $
    isParseError " <<(()>>)"

  , testCase "delimiter mismatch (')')" $
    isParseError " <<())>>)"

  , testCase "incorrect arity" $
    isParseError " <<(?name)>>"

  , testCase "incorrect arity 2" $
    isParseError " <<(?name|text)>>"

  , testCase "Escaping" $
    testTemp "||| <<<( )>>> <<<<(( ))>>>> <<(?undef|| ||| )>>>)>>"
    "| <( )> <<(( ))>>  | )>"

  , testCase "Parenthesis" $
    testTemp " <<(?undef||))>> " " ) "

  , testCase "Invalid name" $
    isParseError " <<(a t)>> "

  , testCase "Template names" $
    testTempWarn " (<<(&f a.txt)>>) " " () " "Template 'f a.txt' not found."

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
    "'hval' is the wrong type: expecting text."

  , testCase "Haskell value (show default)" $
    testTemp " <<(hvalshow)>> " " 1.2 "

  , testCase "Haskell function" $
    testTemp " <<($hfun|hval)>> " " 2 "

  , testCase "Text function" $
    testTemp " <<(!tfun|arst)>> " " ARST "

  , testCase "Showable function" $
    testTemp " <<(showable)>> " " 2 "

  ]

outputTests = testGroup "output and errors" [

    testCase "substitution (undefined)" $
    testTempWarn " (<<(undef)>>) " " () " "name 'undef' not in dictionary."

  , testCase "if (defined)" $
    testTemp "<<(?text|primary <<(text)>>|alternate)>>" "primary textvalue"

  , testCase "if (undefined)" $
    testTemp "<<(?undef|primary|alternate <<(text)>>)>>" "alternate textvalue"

  , testCase "if (true)" $
    testTemp "<<(?true|primary <<(text)>>|alternate)>>" "primary textvalue"

  , testCase "if (false)" $
    testTemp "<<(?false|primary|alternate <<(text)>>)>>" "alternate textvalue"


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
  , ("hval", hVal (1::Int))
  , ("hvaldef", hValDef "(def 1)" (1::Int))
  , ("hvalshow", hValShow (1.2::Float))
  , ("hfun", toValue (pack . show . (+(1::Int))))
  , ("tfun", toValue toUpper)
  , ("showable", toValue (2::Int))
  , ("nillist", toValue ([]::[Text]))
  , ("nildlist", toValue ([]::[Dictionary]))
  , ("html", textValue "<>")
  , ("true", toValue True)
  , ("false", toValue False)
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
