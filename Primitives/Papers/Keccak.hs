{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver        #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise              #-}

module Primitives.Papers.Keccak where

import qualified Prelude as P
import GHC.TypeLits (KnownNat, Nat, type (+), natVal)
import Language.Operators

chi
    :: (Symantics repr)
    => repr 3 -> repr 2 -> repr 1
chi b c =
    let b0 = b ! 0
        b1 = b ! 1
        b2 = b ! 2
        c1 = c ! 0
        c2 = c ! 1
    in b0 + (neg b1) * b2 + b1 * c2 + b2 * c1

chi'
    :: (Symantics repr)
    => repr 5 -> repr 5 -> repr 5
chi' b c =
    let b0 = b ! 0
        b1 = b ! 1
        b2 = b ! 2
        b3 = b ! 3
        b4 = b ! 4
        c0 = c ! 0
        c1 = c ! 1
        c2 = c ! 2
        c3 = c ! 3
        c4 = c ! 4
        a0' = (chi (b0 ++ b1 ++ b2) (c1 ++ c2))
        a1' = (chi (b1 ++ b2 ++ b3) (c2 ++ c3))
        a2' = (chi (b2 ++ b3 ++ b4) (c3 ++ c4))
        a3' = (chi (b3 ++ b4 ++ b0) (c4 ++ c0))
        a4' = (chi (b4 ++ b0 ++ b1) (c0 ++ c1))
    in a0' ++ a1' ++ a2' ++ a3' ++ a4'

chi3
    :: (Symantics repr)
    => (repr 5, repr 5, repr 5) -> (repr 5, repr 5, repr 5)
chi3 (a,b,c) = (chi' b c, chi' c a, chi' a b)