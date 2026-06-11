{-|
Copyright   : (C) 2021-2022, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of Unsigned numbers.
-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE GADTs #-}

module Clash.Falsify.Sized.Unsigned
  ( genUnsigned
  , SomeUnsigned(..)
  , genSomeUnsigned
  ) where

import GHC.TypeNats
  hiding (SNat)
import Test.Falsify.Generator (Gen)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range as Range

import Clash.Promoted.Nat
import Clash.Sized.Internal.Unsigned
import Clash.Falsify.Sized.EdgeCase

genUnsignedEdgeCase :: forall n. KnownNat n => Gen (EdgeCase (Unsigned n))
genUnsignedEdgeCase = Gen.frequency
  [ (1, pure MinBound')
  , (1, AroundMinBound <$> Gen.inRange smallOffset)
  , (1, AroundMaxBound <$> Gen.inRange smallOffset)
  , (7, Arbitrary      <$> Gen.inRange Range.uniform)
  ]

genUnsigned :: forall n. KnownNat n => Gen (Unsigned n)
genUnsigned = toValue <$> genUnsignedEdgeCase

{-------------------------------------------------------------------------------
  SomeUnsigned
-------------------------------------------------------------------------------}

data SomeUnsigned atLeast where
  SomeUnsigned :: SNat n -> Unsigned (atLeast + n) -> SomeUnsigned atLeast

instance KnownNat atLeast => Show (SomeUnsigned atLeast) where
  show (SomeUnsigned SNat x) = show x

genSomeUnsigned
  :: forall atLeast
   . KnownNat atLeast
  => Range.Range Natural
  -> Gen (SomeUnsigned atLeast)
genSomeUnsigned rangeSigned =
  Gen.bindIntegral (Gen.inRange rangeSigned) $ \numExtra ->
    case someNatVal numExtra of
      SomeNat proxy -> SomeUnsigned (snatProxy proxy) <$> genUnsigned
