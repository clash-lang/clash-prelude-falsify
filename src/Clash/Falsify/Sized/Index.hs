{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Clash.Falsify.Sized.Index where

import Data.List.NonEmpty

import qualified Test.Falsify.Generator as F
import Test.Falsify.Range

import Clash.Prelude

genIndex :: forall (n :: Nat). KnownNat n => Range (Index n) -> F.Gen (Index n)
genIndex = F.inRange

genIndexUniform :: forall (n :: Nat). KnownNat n => F.Gen (Index n)
genIndexUniform = genIndex uniform

data IndexWithEdges (n :: Nat) =
      Zero
    | NearMax Int
    | InRange (Index n)

genIndexWithEdges :: forall (n :: Nat). KnownNat n => F.Gen (IndexWithEdges n)
genIndexWithEdges = do
    constr <- F.elem $ 0 :| [1, 2]
    case (constr :: Int) of
        0 -> return Zero
        1 -> NearMax <$> F.inRange (inclusive (0, 3))
        2 -> InRange <$> F.inRange (inclusive (minBound, maxBound))
        _ -> return Zero

indexWithEdgesToIndex :: forall (n :: Nat). (KnownNat n) => IndexWithEdges n -> Index n
indexWithEdgesToIndex Zero = 0
indexWithEdgesToIndex (NearMax i) =
    if i < 0
        then 0
        else
            let n = natToNum @n :: Int
            in fromIntegral (i `mod` n) :: Index n
indexWithEdgesToIndex (InRange i) = i

genIndex' :: forall (n :: Nat). (KnownNat n) => F.Gen (Index n)
genIndex' = indexWithEdgesToIndex <$> genIndexWithEdges
