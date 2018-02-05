{-# LANGUAGE DataKinds #-}

module Primitives.UnmaskedSBOX (sbox') where

import Prelude
import Language.Operators
import Primitives.Math

sbox' :: (Symantics repr) => repr 8 -> repr 8
sbox' a = aff_trans $ gfMapInv $ inv8 (gfMap a)
