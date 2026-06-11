module Main (main) where

import Test.Tasty
import Test.Tasty.Falsify

import Test.Falsify
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P

import Clash.Falsify.Signal ()

main :: IO ()
main = defaultMain $ testGroup "clash-prelude-falsify" [
      testGroup "Sanity" [
          testProperty "minimum" (prop_bool_minimum False)
        ]
    ]

prop_bool_minimum :: Bool -> Property ()
prop_bool_minimum target =
    testMinimum (P.expect target) $ do
      b <- gen $ Gen.bool target
      testFailed b
