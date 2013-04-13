{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Stencil.Internal where

import           Data.Dynamic
import qualified Data.Map     as Map
import           Data.Text    (Text, pack)

if' :: Bool -> a -> a -> a
if' True  a _ = a
if' False _ b = b

-- | Maps from Text keys to 'Value'
newtype Dictionary = Dictionary (Map.Map Name Value)
type Name = Text

-- | Values used for substitutions.
data Value = TxtFunc (Text -> Text)
           | ValFunc (Dynamic -> Either Text Text)
           | Dict Dictionary
           | List [Value]
           | DictList [Dictionary]
           | HVal Dynamic
           | HValDef Text Dynamic

-- | Class of associative datastructures that can become dictionaries.
class ToDict a where
  toDict :: a -> Dictionary

instance ToDict (Map.Map Text Value) where
  toDict = Dictionary
instance ToDict [(Text, Value)] where
  toDict = Dictionary . Map.fromList

-- |Class of things that can be made into a Value.
class ToValue a where
  toValue :: a -> Value

-- | Used in substitutions ( @\<\<(name)\>\>@ ).
instance ToValue Text where
  toValue = textValue
instance ToValue String where
  toValue = textValue . pack
-- | Used in context substitutions ( @\<\<(%name|text)\>\>@ ).
instance ToValue Dictionary where
  toValue = Dict
-- | Shortcut for @toValue . toDict@.
instance ToValue (Map.Map Text Value) where
  toValue = Dict . toDict
-- | Shortcut for @toValue . toDict@.
instance ToValue [(Text, Value)] where
  toValue = Dict . toDict
-- | Lists of dictionaries; used in list substitutions.
-- ( @\<\<(\@name|text|alternate)\>\>@ )
instance ToValue [Dictionary] where
  toValue = DictList
-- | Lists of text; used in list substitutions.
instance ToValue [Text] where
  toValue = List . fmap textValue
-- | Shortcut for @toValue . pack . show@.
instance Show a => ToValue a where
  toValue = textValue . pack . show
-- | Used in text function application ( @\<\<($function|text)\>\>@ ).
instance ToValue (Text -> Text) where
  toValue = TxtFunc
-- | Used in haskell function application ( @\<\<(!function|value)\>\>@ ).
instance Typeable a => ToValue (a -> Text) where
  toValue f = ValFunc (maybe (Left "Wrong type.") (Right . f) . fromDynamic)

-- | A convenience function for use with OverloadedStrings
textValue :: Text -> Value
textValue a = HValDef a (toDyn a)

-- | Arbitrary haskell values. Not an instance of 'ToValue' to prevent
-- ambiguous instances. This can only be used as an argument to a haskell
-- function block ( @\<\<(!function|value)\>\>@ ).
hValue :: Typeable a => a -> Value
hValue = HVal . toDyn

-- | Arbitrary value with a default textual representation; can be used
-- in substitution, list, and both types of function blocks.
hValueWithDefault :: Typeable a => Text -> a -> Value
hValueWithDefault t v = HValDef t (toDyn v)
