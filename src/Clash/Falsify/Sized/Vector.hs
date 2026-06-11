{-|
Copyright   : (C) 2026, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of vectors.
-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Falsify.Sized.Vector
  ( genVec
  , genNonEmptyVec
  , SomeVec(..)
  , genSomeVec
  ) where

import Prelude hiding (repeat)

import Clash.Promoted.Nat
import Clash.Sized.Vector hiding (foldr)
import Data.Maybe (catMaybes)
import GHC.Natural (wordToNatural)
import GHC.TypeNats hiding (SNat)
import Test.Falsify.Generator
import Test.Falsify.Marked
import Test.Falsify.Range

-- | Generate a potentially empty vector, where each element is produced
-- using the supplied generator. For a non-empty vector, see 'genNonEmptyVec'.
--
genVec :: forall n a. (KnownNat n) => Gen a -> Gen (Vec n a)
genVec genElem = repeat <$> genElem

-- | Generate a non-empty vector, where each element is produced using the
-- supplied generator. For a potentially empty vector, see 'genVec'.
--
genNonEmptyVec :: forall n a. (KnownNat n, 1 <= n) => Gen a -> Gen (Vec n a)
genNonEmptyVec = genVec

data SomeVec atLeast a where
  SomeVec :: KnownNat n => SNat n -> Vec (atLeast + n) a -> SomeVec atLeast a

instance (KnownNat atLeast, Show a) => Show (SomeVec atLeast a) where
  show (SomeVec SNat xs) = show xs

instance (KnownNat atLeast) => Functor (SomeVec atLeast) where
  fmap fn (SomeVec snat xs) = SomeVec snat $ fmap fn xs

instance (KnownNat atLeast) => Foldable (SomeVec atLeast) where
  foldMap m (SomeVec _ xs) = foldMap m xs
  foldr fn ini (SomeVec _ xs) = foldr fn ini xs

instance (KnownNat atLeast) => Traversable (SomeVec atLeast) where
  traverse fn (SomeVec snat xs) = SomeVec snat <$> traverse fn xs
  sequenceA (SomeVec snat xs) = SomeVec snat <$> sequenceA xs

genSomeVec
  :: forall atLeast a
   . (KnownNat atLeast)
  => Range Natural
  -> Gen a
  -> Gen (SomeVec atLeast a)
genSomeVec rangeElems genElem = do
  numExtra <- inRange rangeElems
  (marks :: SomeVec atLeast (Marked Gen a)) <- case someNatVal numExtra of
    SomeNat proxy -> sequenceA $ SomeVec (snatProxy proxy) $ repeat $ mark genElem
  (maybes :: SomeVec atLeast (Maybe a)) <- selectAllKept marks
  let
    numJust :: Natural
    numJust = wordToNatural (countKept marks) - natToNum @atLeast

    someVecToList :: SomeVec atLeast b -> [b]
    someVecToList (SomeVec _ vec) = toList vec

    someVec :: SomeVec atLeast a
    someVec =
      case someNatVal numJust of
        SomeNat proxy ->
          SomeVec (snatProxy proxy) $ unsafeFromList $ catMaybes $ someVecToList maybes

  return someVec
