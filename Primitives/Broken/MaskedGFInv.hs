{-# LANGUAGE DataKinds #-}

module Primitives.Broken.MaskedGFInv 
    (m_invGF)
where

import Prelude (($))

import Language.Operators
import Primitives.Math
import Primitives.Math22


m_invGF :: (Symantics repr) => (repr 8, repr 4) -> repr 8 -> repr 8
m_invGF = m_inv8


----------------------------------------
-- GF(16) x GF(16) masked inversion   --
-- GF((2^4)^2) : n(x) = x^2 + x + {e} --
----------------------------------------

m_inv8 :: (Symantics repr) => (repr 8, repr 4) -> repr 8 -> repr 8
m_inv8 (m, m_fresh) am = gfMapInv (f_amh, f_aml)
  where
    (mh, ml)     = gfMap m
    (am_h, am_l) = gfMap am
    m_fresh_low  = (m_fresh ! 1) ++ (m_fresh ! 0)

    -- Note: order is important!
    -- f_ah = ah' + mh
    -- f_al = al' + ml
    -- f_d  = d + mh
    -- f_d' = d' + mh
    f_amh  = t4 + t5
    f_aml  = t11 + t12 -- BROKEN! (No fresh mask)
    f_dm   = t20 + t21
    f_dmh' = m_inv4 (mh, m_fresh_low) f_dm
    f_dml' = f_dmh' + t8

    -- sum terms --
    t1  = c1 + m_fresh
    t2  = c6 + c5
    t3  = t1 + t2
    t4  = dm4 + m_fresh
    t5  = c7 + t3
    t6  = c2 -- fresh mask removed
    t7  = t6 + c5
    t8  = c9 + c6
    t9  = dm5 -- fresh mask removed
    t10 = t7 + t8
    t11 = f_amh + t9
    t12 = c8 + t10
    t13 = dm1 + m_fresh
    t14 = c1 + m_fresh
    t15 = c3 + c4
    t16 = c5 + c6
    t17 = t13 + dm2
    t18 = t14 + c2
    t19 = t15 + t16
    t20 = t17 + dm3
    t21 = t18 + t19

    -- (masked) sensitive data --
    dm1a = sq4 am_h
    dm1  = mulP0 dm1a
    dm2  = am_h ..* am_l
    dm3  = sq4 am_l
    dm4  = am_h ..* f_dml'
    dm5  = am_l ..* f_dmh'

    -- correction terms --
    c1  = am_h ..* ml
    c2  = am_l ..* mh
    c3  = mulP0 c31
    c31 = sq4 mh
    c4  = sq4 ml
    c5  = mh ..* ml
    c6  = mh
    c7  = f_dml' ..* mh
    c8  = f_dmh' ..* ml
    c9  = ml


------------------------------------
-- GF(4) x GF(4) masked inversion --
------------------------------------

-- m_inv4 :: (Symantics repr) => (repr 4, repr 2) -> repr 4 -> repr 4
m_inv4 (m, m_fresh) am = gfMapInv22 (f_amh, f_aml)
  where
    (am_h, am_l) = gfMap22 am
    (mh, ml)     = gfMap22 m

    -- Note: order is important!
    -- f_ah = ah' + mh
    -- f_al = al' + ml
    -- f_d  = d + mh
    -- f_d' = d' + mh
    f_amh  = t4 + t5
    f_aml  = t11 + t12
    f_dm   = t20  +  t21
    dm_inv = inv2 f_dm -- inversion is linear in GF(4): (d+mh)^-1 (d+mh)^2 = d^2 + mh^2 = d^-1 + mh^2
    f_dmh' = dm_inv + (c31 + mh)
    f_dml' = dm_inv + (c31 + ml)

    -- sum terms --
    t1  = c1 + m_fresh
    t2  = c6 + c5
    t3  = t1 + t2
    t4  = dm4 + m_fresh
    t5  = c7 + t3
    t6  = c2 + m_fresh
    t7  = t6 + c5
    t8  = c9 + c6
    t9  = dm5 + m_fresh
    t10 = t7 + t8
    t11 = f_amh + t9
    t12 = c8 + t10
    t13 = dm1 + m_fresh
    t14 = c1 + m_fresh
    t15 = c3 + c4
    t16 = c5 + c6
    t17 = t13 + dm2
    t18 = t14 + c2
    t19 = t15 + t16
    t20 = t17 + dm3
    t21 = t18 + t19


    -- (masked) sensitive data --
    dm1a = sq2 am_h
    dm1  = mul2co dm1a
    dm2  = am_h .*. am_l
    dm3  = sq2 am_l
    dm4  = am_h .*. f_dml'
    dm5  = am_l .*. f_dmh'

    -- correction terms --
    c1  = am_h .*. ml
    c2  = am_l .*. mh
    c31 = sq2 mh
    c3  = mul2co c31
    c4  = sq2 ml
    c5  = mh .*. ml
    c6  = mh
    c7  = f_dml' .*. mh
    c8  = f_dmh' .*. ml
    c9  = ml
