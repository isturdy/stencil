{-# LANGUAGE OverloadedStrings #-}

module Text.Stencil (
         evalTemplate
       , Value()
       , ToValue( toValue )
       , textValue
       , hVal
       , hValDef
       , hValShow
       , Dictionary
       , ToDict( toDict )
       , Context
       , Template
       , Templates
       , Warnings
       ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Writer
import           Data.Attoparsec.Text
import qualified Data.Map                     as Map
import           Data.Text                    (Text, pack)
import qualified Data.Text                    as T
import           Text.XHtmlCombinators.Escape (escape)

import           Text.Stencil.Internal

-- Syntax tree
newtype Block = Block [Element]
                deriving (Show)

type TemplateName = Text

data PreEscaped = Escaped
                | NotEscaped
                  deriving (Show, Eq)

data Element = ElText Text
             | ElSubs PreEscaped Name
             | ElIf   PreEscaped Name Block Block
             | ElDict PreEscaped Name Block
             | ElList PreEscaped Name Block Block
             | ElVFun PreEscaped Name Name
             | ElFun  PreEscaped Name Block
             | ElTemp PreEscaped TemplateName
               deriving (Show)

-- Parser
notSpecialChar :: Char -> Bool
notSpecialChar '<' = False
notSpecialChar '|' = False
notSpecialChar ')' = False
notSpecialChar _   = True

testChar :: (Char -> Bool) -> Parser ()
testChar f = do mc <- peekChar
                case mc of
                  Nothing -> return ()
                  Just c  -> if' (f c) (return ()) (fail "")

notSpecial :: Parser Text
notSpecial = T.concat <$> many1'
             (takeWhile1 notSpecialChar
             <|> "||" .*> takeWhile1 (== '|')
             <|> T.snoc <$> ("<<" .*> takeWhile1 (== '<')) <*> char '('
             <|> T.cons <$> char ')' <*> (">>" .*> takeWhile1 (== '>'))
             <|> T.append <$> string "))" <*> (">>" .*> takeWhile1 (== '>'))
             <|> T.singleton <$> (char ')' <* testChar (notInClass ")>")))

parseTemp :: Text -> Either Text Block
parseTemp t = case feed (parse (Block <$> many' element) t) "" of
  Done "" r  -> Right r
  Done tx _  -> Left $ "Parse error at " <> tx
  Fail _ _ e -> Left $ pack e
  Partial _  -> error "This is impossible (parser failed to complete)."

element :: Parser Element
element = (ElText <$> notSpecial)
          <|> tag

name :: Parser Name
name = testChar (notInClass "?%@$!&") *> (T.cons <$> letter <*>
                 (T.pack <$> many' (letter <|> digit <|> char '_'
                                    <|> char '\''))
                 <|> return "")


templateName :: Parser TemplateName
templateName = notSpecial <|> return ""

tag :: Parser Element
tag = string "<<(" >> do
  esc <- option Escaped (const NotEscaped <$> char '(')
  let close = if' (esc == Escaped) ")>>" "))>>"
  let block = Block <$>
              many' (if' (esc == NotEscaped) element
                     (element <|> (((ElText . T.singleton) <$>
                                    (char ')' <* testChar (/= '>'))))))
  let pipeBlock = char '|' *> block
  ElIf esc <$> (char '?' *> name) <*> pipeBlock <*> pipeBlock <* close
    <|> ElDict esc <$> (char '%' *> name) <*> pipeBlock <* close
    <|> ElList esc <$> (char '@' *> name) <*> pipeBlock <*> pipeBlock <* close
    <|> ElVFun esc <$> (char '$' *> name) <*> (char '|' *> name)  <* close
    <|> ElFun  esc <$> (char '!' *> name) <*> pipeBlock <* close
    <|> ElTemp esc <$> (char '&' *> templateName) <* close
    <|> ElSubs esc <$> name <* close

-- Renderer
escapeHtml :: PreEscaped -> Text -> Text
escapeHtml e = if' (e == NotEscaped) escape id

type Context = [Dictionary]
lookupInContext :: Context -> Name -> Maybe Value
lookupInContext [] _ = Nothing
lookupInContext (Dictionary d:ds) n =
  Map.lookup n d `mplus` lookupInContext ds n

type Warnings = [Text]
warn :: Text -> Writer Warnings Text
warn t = tell [t] >> return ""

warnNotFound :: Name -> Writer Warnings Text
warnNotFound n = warn ("name '" <> n <> "' not in dictionary.")

warnWrongType :: Name -> Text ->  Writer Warnings Text
warnWrongType n t = warn ("'" <> n
                          <> "' is the wrong type: expecting " <> t <> ".")

subsBlock :: Templates -> Context -> Block -> Writer Warnings Text
subsBlock t c (Block b) = liftM T.concat (mapM (substitute t c) b)

subsBlockWithDict :: Templates -> Dictionary -> Context -> Block ->
                     Writer Warnings Text
subsBlockWithDict t d c = subsBlock t (d:c)

subsBlockWithDef :: Templates -> Value -> Context -> Block ->
                    Writer Warnings Text
subsBlockWithDef t txt = subsBlockWithDict t singletonDict
  where singletonDict = Dictionary $ Map.fromList [("", txt)]

type Template = Text
-- | Maps of templates; used for include blocks ( @\<\<(&template)\>\>@ ).
type Templates = Map.Map Text Template

substitute :: Templates -> Context -> Element -> Writer Warnings Text
substitute _ _ (ElText n) = return n
substitute _ c (ElSubs e n) = liftM (escapeHtml e) $
  case lookupInContext c n of
    Nothing            -> warnNotFound n
    Just (HValDef t _) -> return t
    _                  -> warnWrongType n "text"
substitute t c (ElIf e n bt bf) = liftM (escapeHtml e) $
  case lookupInContext c n of
    Nothing            -> subsBlock t c bf
    Just (HBool False) -> subsBlock t c bf
    _                  -> subsBlock t c bt
substitute t c (ElDict e n b) = liftM (escapeHtml e) $
  case lookupInContext c n of
    Nothing            -> warnNotFound n
    Just (Dict d)      -> subsBlockWithDict t d c b
    _                  -> warnWrongType n "dictionary"
substitute t c (ElList e n b bnil) = liftM (escapeHtml e) $
  case lookupInContext c n of
    Nothing            -> warnNotFound n
    Just (List [])     -> subsBlock t c bnil
    Just (List l)      -> liftM T.concat $ mapM
                          (\txt -> subsBlockWithDef t txt c b) l
    Just (DictList []) -> subsBlock t c bnil
    Just (DictList l)  -> liftM T.concat $ mapM
                          (\d -> subsBlockWithDict t d c b) l
    _                  -> warnWrongType n "list"
substitute t c (ElFun e n b) = liftM (escapeHtml e) $
  case lookupInContext c n of
    Nothing            -> warnNotFound n
    Just (TxtFunc f)   -> liftM f (subsBlock t c b)
    _                  -> warnWrongType n "text function"
substitute _ c (ElVFun e fn vn) = liftM (escapeHtml e) $
  case lookupInContext c fn of
    Nothing          -> warnNotFound fn
    Just (ValFunc f) -> case lookupInContext c vn of
      Nothing           -> warnNotFound vn
      Just (HVal v)     -> case f v of
        Right t            -> return t
        Left err           -> warn ("Function " <> fn <> "failed with error '"
                              <> err <> ".")
      _                 -> warnWrongType vn "Haskell value"
    _                -> warnWrongType fn"Haskell function"
substitute ts c (ElTemp e n) = liftM (escapeHtml e) $
  case Map.lookup n ts of
    Nothing -> warn $ "Template '" <> n <> "' not found."
    Just t  -> case evalTemplate ts c t of
      Left err        -> warn err
      Right (txt, ws) -> mapM_ warn ws >> return txt

-- | Evaluates templates. Returns @'Left' 'Text'@ if it cannot parse the
-- template, and @'Right' ('Text', 'Warnings')@ if it can. If it encounters
-- non-fatal template errors (such as invalid names or wrong types) it will
-- do the best it can and include descriptions of the problems.
evalTemplate :: Templates -> Context -> Template ->
                Either Text (Text, Warnings)
evalTemplate ts c t = case parseTemp t of
  Left  e -> Left e
  Right b -> Right . runWriter $ subsBlock ts c b
