{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE UndecidableInstances #-}
module Text.Stencil where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Writer
import           Data.Attoparsec.Text
import           Data.Dynamic
import qualified Data.Map             as Map
import           Data.Text            (Text, pack)
import qualified Data.Text            as T
import           Prelude              hiding (concatMap)

if' :: Bool -> a -> a -> a
if' True  a _ = a
if' False _ b = b

type Dictionary = Map.Map Name Value

data Value = Txt Text
           | TxtFunc (Text -> Text)
           | ValFunc (Dynamic -> Either Text Text)
           | Dict Dictionary
           | List [Text]
           | DictList [Dictionary]
           | HVal Dynamic
           | HValDef Text Dynamic

-- |Class of associative datastructures that can become dictionaries
class ToDict a where
  toDict :: a -> Dictionary

-- |Class of things that can be made into a Value.
class ToValue a where
  toValue :: a -> Value

instance ToValue Text where
  toValue = Txt
instance ToValue Dictionary where
  toValue = Dict
instance ToDict a => ToValue [a] where
  toValue = DictList . fmap toDict
instance ToValue [Text] where
  toValue = List

-- |Arbitrary values. Not an instance due to overlapping; use these to
-- write instances for specific types as needed.
hValueWithDefault :: Typeable a => Text -> a -> Value
hValueWithDefault t v = HValDef t (toDyn v)

hValue :: Typeable a => a -> Value
hValue = HVal . toDyn

instance ToValue (Text -> Text) where
  toValue = TxtFunc

instance Typeable a => ToValue (a -> Text) where
  toValue f = ValFunc (maybe (Left "Wrong type.") (Right . f) . fromDynamic)

-- |The catch-nearly-all
instance Show a => ToValue a where
  toValue = Txt . pack . show

-- |Syntax tree
newtype Block = Block [Element]
                deriving (Show)

type Name = Text

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
             | ElTemp PreEscaped Name
               deriving (Show)

notSpecialChar :: Char -> Bool
notSpecialChar '<' = False
notSpecialChar '|' = False
notSpecialChar '>' = False
notSpecialChar _   = True

notSpecial :: Parser Text
notSpecial = T.concat <$> many1'
             (takeWhile1 notSpecialChar
             <|> "||" .*> takeWhile1 (== '|')
             <|> "<<<" .*> takeWhile1 (== '<')
             <|> "<<<" .*> takeWhile1 (== '<')
             <|> ">>>" .*> takeWhile1 (== '>'))

-- | Parser
parseTemp :: Text -> Either Text Block
parseTemp t = case p t of
  Done _ r   -> Right r
  Fail _ _ e -> Left $ pack e
  Partial _  -> error "This is impossible (parser failed to complete."
  where p txt = case parse (Block <$> many' element) txt of
          Partial f -> f ""
          x         -> x

element :: Parser Element
element = (ElText <$> notSpecial)
          <|> tag

block :: Parser Block
block = Block <$> many' element

name :: Parser Name
name = notSpecial <|> return ""

pipeBlock :: Parser Block
pipeBlock = char '|' *> block

tag :: Parser Element
tag = string "<<" >> do
  esc <- option Escaped (const NotEscaped <$> char '<')
  let close = if esc == Escaped then ">>" else ">>>"
  (ElIf   esc <$> (char '?' *> name) <*> pipeBlock <*> pipeBlock <* close
   <|> ElDict esc <$> (char '%' *> name) <*> pipeBlock <* close
   <|> ElList esc <$> (char '@' *> name) <*> pipeBlock <*> pipeBlock <* close
   <|> ElVFun esc <$> (char '$' *> name) <*> (char '|' *> name)  <* close
   <|> ElFun  esc <$> (char '!' *> name) <*> pipeBlock <* close
   <|> ElTemp esc <$> (char '&' *> name) <* close
   <|> ElSubs esc <$> name <* close)

-- | Renderer

escapeHtml :: PreEscaped -> Text -> Text
escapeHtml e = if' (e == NotEscaped) undefined id

type Context = [Dictionary]
lookupInContext :: Context -> Name -> Maybe Value
lookupInContext [] _ = Nothing
lookupInContext (d:ds) n = Map.lookup n d `mplus` lookupInContext ds n

pushContext :: Dictionary -> Context -> Context
pushContext = (:)

type Warnings = [Text]
warn :: Text -> Writer Warnings Text
warn t = tell [t] >> return ""

warnNotFound :: Name -> Writer Warnings Text
warnNotFound n = warn ("name '" <> n <> "'not in dictionary.")

warnWrongType :: Name -> Text ->  Writer Warnings Text
warnWrongType n t = warn ("'" <> n <> "'refers to a value of the wrong type."
                        <> " Expecting " <> t <> ".")

subsBlock :: Templates -> Context -> Block -> Writer Warnings Text
subsBlock t c (Block b) = liftM T.concat (mapM (substitute t c) b)

subsBlockWithDict :: Templates -> Dictionary -> Context -> Block ->
                     Writer Warnings Text
subsBlockWithDict t d c = subsBlock t (d:c)

subsBlockWithDef :: Templates -> Text -> Context -> Block ->
                    Writer Warnings Text
subsBlockWithDef t txt c = subsBlock t (Map.fromList [("",Txt txt)]:c)

type Template = Text
type Templates = Map.Map Text Template

substitute :: Templates -> Context -> Element -> Writer Warnings Text
substitute _ _ (ElText n) = return n
substitute _ c (ElSubs e n) = liftM (escapeHtml e) $
  case lookupInContext c n of
    Nothing            -> warnNotFound n
    Just (Txt t)       -> return t
    Just (HValDef t _) -> return t
    _                  -> warnWrongType n "text"
substitute t c (ElIf e n bt bf) = liftM (escapeHtml e) $
  case lookupInContext c n of
    Nothing            -> subsBlock t c bt
    _                  -> subsBlock t c bf
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
    Nothing -> warn $ "Template " <> n <> " not found."
    Just t  -> case evalTemplate ts c t of
      Left err        -> warn err
      Right (txt, ws) -> mapM_ warn ws >> return txt


evalTemplate :: Templates -> Context -> Template ->
                Either Text (Text, Warnings)
evalTemplate ts c t = case parseTemp t of
  Left  e -> Left e
  Right b -> Right . runWriter $ subsBlock ts c b
