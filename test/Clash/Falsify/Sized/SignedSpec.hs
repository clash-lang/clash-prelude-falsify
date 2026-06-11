module Clash.Falsify.Sized.SignedSpec (tests) where

import Test.Tasty
import Test.Tasty.Falsify
import Test.Falsify (gen, assert)
import qualified Test.Falsify.Predicate as P

import Clash.Falsify.Sized.Signed  (genSigned)
import Clash.Sized.Internal.Signed (Signed)

tests :: TestTree
tests = testGroup "Signed"
  [ testProperty "always in bounds" $ do
      x <- gen (genSigned @8)
      assert $ P.between minBound maxBound .$ x
  ]
