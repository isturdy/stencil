{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Stencil.Generic where

import Data.Data
import qualified Data.Map as Map
import Data.Typeable

import Text.Stencil

data Foo = Foo {
         bar :: Int,
         baz :: String
         } deriving (Show, Data, Typeable)

f = Foo 1 "bar"

d = gmapQ genValue (1::Int)
  where genValue x
          | typeOf x == typeOf (1::Int) = Just (dataTypeName, 1)
          | otherwise                   = Nothing


genToValues :: (Data a) => a -> [Maybe Value]
genToValues = gmapQ genValue
  where genValue x
          | typeOf x == typeOf (1::Int) = Just (toValue x)
          | otherwise = Nothing


recDict r = case r of
  (AlgConstr _) -> Map.fromList $ zip (constrFields $ toConstr r) (genToValues r)