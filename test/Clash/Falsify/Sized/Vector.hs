{-|
Copyright   : (C) 2026, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Test random generation of vectors.
-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Falsify.Sized.Vector where

import Clash.Falsify.Sized.Vector

prop_vec_empty :: forall n a. Eq a => Vec n a -> Property ()
prop_vec_empty target =
    testMinimum (P.expect target) $ do
        vec <- gen $ G
