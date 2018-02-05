{-# LANGUAGE DataKinds #-}

module Primitives.Math 
  ( (..*), mul4, sq4, mulP0
  , gfMap, gfMapInv, aff_trans
  , inv4, inv8, invGF
  , get4, get8l, get8h
  )
where

import Prelude            hiding ((+), (*), (++))
import Language.Operators


infixl 7  ..*

(..*) :: (Symantics repr) => repr 4 -> repr 4 -> repr 4
(..*) = mul4

-------------------------------------------------------------------------------

--(..*) a b = pack4 q3 q2 q1 q0
mul4 a b = q3 ++ q2 ++ q1 ++ q0
  where
    q0  = a0 * b0 + a3  * b1  + a2  * b2 + a1 * b3
    q1  = a1 * b0 + a_A * b1  + a_B * b2 + (a1 + a2) * b3
    q2  = a2 * b0 + a1  * b1  + a_A * b2 + a_B * b3
    q3  = a3 * b0 + a2  * b1  + a1  * b2 + a_A * b3

    a_A = a0 + a3
    a_B = a2 + a3

    (a3, a2, a1, a0) = get4 a
    (b3, b2, b1, b0) = get4 b

-------------------------------------------------------------------------------

-- | Constant multiplication, p0 = 0xe :: Unsigned 4
mulP0 a = q3 ++ q2 ++ q1 ++ q0
  where
    q0 = a1  + a_B
    q1 = a_A
    q2 = a_A + a2
    q3 = a_A + a_B

    a_A = a0 + a1
    a_B = a2 + a3

    (a3, a2, a1, a0) = get4 a

-------------------------------------------------------------------------------

sq4 a = q3 ++ q2 ++ q1 ++ q0
  where
    q0 = a0 + a2
    q1 = a2
    q2 = a1 + a3
    q3 = a3

    (a3, a2, a1, a0) = get4 a

-------------------------------------------------------------------------------

aff_trans a = up ++ dw
  where
    up = q7 ++ q6 ++ q5 ++ q4
    dw = q3 ++ q2 ++ q1 ++ q0

    q0 = neg a0 + a_C + a_D
    q1 = neg a5 + a_A + a_D
    q2 =  a2  + a_A + a_D
    q3 =  a7  + a_A + a_B
    q4 =  a4  + a_A + a_B
    q5 = neg a1 + a_B + a_C
    q6 = neg a6 + a_B + a_C
    q7 =  a3  + a_C + a_D

    a_A = a0 + a1
    a_B = a2 + a3 
    a_C = a4 + a5
    a_D = a6 + a7

    (a7, a6, a5, a4) = get8h a
    (a3, a2, a1, a0) = get8l a

-------------------------------------------------------------------------------

gfMapInv (a_h, a_l) = a7 ++ a6 ++ a5 ++ a4 ++ a3 ++ a2 ++ a1 ++ a0
  where
    --a = packU8 (packU4 a7 a6 a5 a4) (packU4 a3 a2 a1 a0)
    a7 = a_B  + a_l2 + a_h3
    a6 = a_A  + a_l2 + a_l3 + a_h0
    a5 = a_B  + a_l2
    a4 = a_A  + a_B  + a_l3
    a3 = a_B  + a_l1 + a_h2
    a2 = a_A  + a_B
    a1 = a_B  + a_h3
    a0 = a_l0 + a_h0

    a_A = a_l1 + a_h3
    a_B = a_h0 + a_h1

    (a_h3, a_h2, a_h1, a_h0) = get4 a_h
    (a_l3, a_l2, a_l1, a_l0) = get4 a_l

-------------------------------------------------------------------------------

gfMap a = (a_h, a_l)
  where
    a_h = a_h3 ++ a_h2 ++ a_h1 ++ a_h0
    a_l = a_l3 ++ a_l2 ++ a_l1 ++ a_l0

    a_h3 = a_B
    a_h2 = a_B + a2 + a3
    a_h1 = a_A + a_C
    a_h0 = a_C + a5

    a_l3 = a2 + a4
    a_l2 = a_A
    a_l1 = a1 + a2
    a_l0 = a_C + a0 + a5

    a_A = a1 + a7
    a_B = a5 + a7
    a_C = a4 + a6

    (a7, a6, a5, a4) = get8h a
    (a3, a2, a1, a0) = get8l a

-------------------------------------------------------------------------------

-- | Not used in the masked primitive (replaced by m_inv4).
inv4 a = q3 ++ q2 ++ q1 ++ q0
  where
    q0  = a_A     + a0      + a0 * a2  + a1 * a2 + a0 * a1 * a2
    q1  = a0 * a1 + a0 * a2 + a3       + a1 * a2 + a1 * a3       + a0 * a1 * a3
    q2  = a0 * a1 + a0 * a2 + a3       + a2      + a0 * a3       + a0 * a2 * a3
    q3  = a_A     + a0 * a3 + a1 * a3 + a2 * a3

    a_A = a1 + a2 + a3 + a1 * a2 * a3

    (a3, a2, a1, a0) = get4 a

-------------------------------------------------------------------------------

-- | Not used in the masked primitive (replaced by m_inv8).
--   (where p0 = literal4 0xe)
inv8 (a_h, a_l) = (q_h, q_l)
  where
    q_h = mul4 a_h d'
    q_l = mul4 (a_h + a_l) d'

    d  = (mulP0 $ sq4 a_h) + (mul4 a_h a_l) + (sq4 a_l)
    d' = inv4 d

-------------------------------------------------------------------------------

-- | Not used in the masked primitive (replaced by m_inv8).
--invGF 0 = 0
invGF a = gfMapInv $ inv8 (gfMap a)


---------------
-- Utilities --
---------------

get4 :: Symantics repr => repr 4 -> (repr 1, repr 1, repr 1, repr 1)
get4 v  = (v ! 3, v ! 2, v ! 1, v ! 0)

get8l :: Symantics repr => repr 8 -> (repr 1, repr 1, repr 1, repr 1)
get8l v = (v ! 3, v ! 2, v ! 1, v ! 0)

get8h :: Symantics repr => repr 8 -> (repr 1, repr 1, repr 1, repr 1)
get8h v = (v ! 7, v ! 6, v ! 5, v ! 4)

