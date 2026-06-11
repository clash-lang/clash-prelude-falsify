module Clash.Falsify.Sized.EdgeCase
  ( EdgeCase(..)
  , toValue
  , smallOffset
  ) where

import qualified Test.Falsify.Range as Range

data EdgeCase a
  = MinBound'
  | AroundMinBound Word
  | AroundMaxBound Word
  | Arbitrary a

toValue :: (Bounded a, Num a) => EdgeCase a -> a
toValue = \case
  MinBound'        -> minBound
  AroundMinBound n -> minBound + fromIntegral n
  AroundMaxBound n -> maxBound - fromIntegral n
  Arbitrary x      -> x

smallOffset :: Range.Range Word
smallOffset = Range.inclusive (0, 4)
