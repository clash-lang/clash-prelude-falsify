module Clash.Falsify.Signal where
import Clash.Signal.Internal
import Test.Falsify
import qualified Test.Falsify.Generator as Gen
import Data.List.NonEmpty (NonEmpty((:|)))

genSignal :: Gen a -> Gen (Signal dom a)
genSignal genElem = go
 where
  go = liftA2 (:-) genElem go

-- | Shrinks to 'Rising'
genActiveEdge :: Gen ActiveEdge
genActiveEdge = Gen.elem (Rising :| [ Falling])

-- | Shrinks to 'Defined'
genInitBehavior :: Gen InitBehavior
genInitBehavior = Gen.elem (Defined :| [Unknown])

-- | Shrinks to 'Synchronous'
genResetKind :: Gen ResetKind
genResetKind = Gen.elem (Synchronous :| [Asynchronous])

-- | Shrinks to 'ActiveHigh'
genResetPolarity :: Gen ResetPolarity
genResetPolarity = Gen.elem (ActiveHigh :| [ActiveLow])
