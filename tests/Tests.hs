module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

import Text.StencilTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
        stencilTests
        ]
