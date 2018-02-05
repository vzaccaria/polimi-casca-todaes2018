{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DataKinds #-}

module Primitives.Math22 where


import Prelude hiding ((+), (*), (++))
import Primitives.Math    (get4)
import Language.Operators


infixl 7  .*.


-------------------------
-- Operations in GF(4) --
-------------------------

(.*.) :: (Symantics repr) => repr 2 -> repr 2 -> repr 2
(.*.) = mul2

-------------------------------------------------------------------------------

sq2 ::  (Symantics repr) => repr 2 -> repr 2
sq2 a = c1 ++ c0
  where
    c0 = a1 + a0
    c1 = a1

    (a1, a0) = get2 a

-------------------------------------------------------------------------------

mul2 :: (Symantics repr) => repr 2 -> repr 2 -> repr 2
mul2 a b = c1 ++ c0
  where
    c0 = (a0 * b0) + h
    c1 = (a1 * b0) + (a0 * b1) + h
    h  = a1 * b1

    (a1, a0) = get2 a
    (b1, b0) = get2 b

-------------------------------------------------------------------------------

-- | Constant multiplication in GF(4).
--   The GF(4) constant is: lambda^2 = {10}.
mul2co :: (Symantics repr) => repr 2 -> repr 2
mul2co a = c1 ++ c0
  where
    c0 = a1
    c1 = a1 + a0

    (a1, a0) = get2 a

-------------------------------------------------------------------------------

inv2 :: (Symantics repr) => repr 2 -> repr 2
inv2 = sq2


----------------------------------------
-- GF(16) <--> GF(4) x GF(4) mappings --
----------------------------------------

gfMap22 :: (Symantics repr) => repr 4 -> (repr 2, repr 2)
gfMap22 a = (b3 ++ b2, b1 ++ b0)
  where
    b3 = a3
    b2 = a1 + a2
    b1 = a1 + a3
    b0 = a0 + a1

    (a3, a2, a1, a0) = get4 a

-------------------------------------------------------------------------------

gfMapInv22 :: (Symantics repr) => (repr 2, repr 2) -> repr 4
gfMapInv22 (a_h, a_l) = b3 ++ b2 ++ b1 ++ b0
  where
    b3 = a3
    b2 = a1 + a2 + a3
    b1 = a1 + a3
    b0 = a0 + a1 + a3

    (a3, a2) = get2 a_h
    (a1, a0) = get2 a_l


---------------
-- Utilities --
---------------

get2 v  = (v ! 1, v ! 0)
