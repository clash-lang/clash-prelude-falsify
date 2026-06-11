{-|
Copyright   : (C) 2021-2022, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of Signed numbers.
-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE GADTs #-}

module Clash.Falsify.Sized.Signed
  ( genSigned
  , SomeSigned(..)
  , genSomeSigned
  ) where

import GHC.TypeNats
  hiding (SNat)
import Test.Falsify.Generator (Gen)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range as Range

import Clash.Promoted.Nat
import Clash.Sized.Internal.Signed
import Clash.Falsify.Sized.EdgeCase

genSignedEdgeCase :: forall n. KnownNat n => Gen (EdgeCase (Signed n))
genSignedEdgeCase = Gen.frequency
  [ (1, pure MinBound')
  , (1, pure (Arbitrary 0))
  , (1, AroundMinBound <$> Gen.inRange smallOffset)
  , (1, AroundMaxBound <$> Gen.inRange smallOffset)
  , (6, Arbitrary      <$> Gen.inRange (Range.withOrigin (minBound, maxBound) 0))
  ]

genSigned :: forall n. KnownNat n => Gen (Signed n)
genSigned = toValue <$> genSignedEdgeCase

{-------------------------------------------------------------------------------
  SomeSigned
-------------------------------------------------------------------------------}

data SomeSigned atLeast where
  SomeSigned :: SNat n -> Signed (atLeast + n) -> SomeSigned atLeast

instance KnownNat atLeast => Show (SomeSigned atLeast) where
  show (SomeSigned SNat x) = show x

genSomeSigned
  :: forall atLeast
   . KnownNat atLeast
  => Range.Range Natural
  -> Gen (SomeSigned atLeast)
genSomeSigned rangeSigned =
  Gen.bindIntegral (Gen.inRange rangeSigned) $ \numExtra ->
    case someNatVal numExtra of
      SomeNat proxy -> SomeSigned (snatProxy proxy) <$> genSigned
