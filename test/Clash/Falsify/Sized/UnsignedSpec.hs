module Clash.Falsify.Sized.UnsignedSpec (tests) where

import Test.Tasty
import Test.Tasty.Falsify
import Test.Falsify (gen, assert)
import qualified Test.Falsify.Predicate as P

import Clash.Falsify.Sized.Unsigned  (genUnsigned)
import Clash.Sized.Internal.Unsigned (Unsigned)

tests :: TestTree
tests = testGroup "Unsigned"
  [ testProperty "always in bounds" $ do
      x <- gen (genUnsigned @8)
      assert $ P.between minBound maxBound .$ x
  ]
