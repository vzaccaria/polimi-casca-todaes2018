{-# LANGUAGE DataKinds #-}

module Backend.MaskProp.EvalMasked (main) where


import Prelude hiding ((+), (++))
import CLaSH.Sized.Vector  ( Vec(Nil) )
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Set  as S

import Language.Operators
import Backend.MaskProp.Operators
import Primitives.Math22          ((.*.), sq2)
import Primitives.Math            (aff_trans)
import Primitives.MaskedGFInv     (m_invGF)
import Primitives.MaskedSBOX      (msbox)

import qualified Primitives.Broken.MaskedGFInv as Broken (m_invGF)



--------------------------------------------
--------------- Unit tests. ----------------
--------------------------------------------

main = defaultMain testMasked


testMasked :: TestTree
testMasked = testGroup "Test Masking Properties" [
  testGroup "Test basic GF properties"
    [ testCase "sq2" $ assert $ mvalid $ sq2 am2
    , testCase "mul2" $ assert $ mvalid $ am2 .*. am2'
    , testCase "mul2 (INSECURE)" $ assert $ mvalid $ am2 .*. m2
    ]
  ,
  testGroup "Polymorphic Masked S-BOX"
    [ testCase "m_invGF: with fresh mask" $ assert $ predMskdInv
    , testCase "msbox: with fresh mask" $ assert $ predMSBox
    , testCase "msbox: NO fresh mask (INSECURE)" $ assert $ predMSZeroed
    ]
  ,
  testGroup "Mask dependencies as inputs of the Masked S-Box"
    [ testCase "msbox: fresh mask dep. to the higher mask (INSECURE)" $ assert $ predDepHi
    , testCase "msbox: fresh mask dep. to the lower mask" $ assert $ predDepLo -- Apparently still secure...
    , testCase "msbox: mask with repeating bits (INSECURE)" $ assert $ predRepMsk
    ]
  ,
  testGroup "BROKEN Polymorphic Masked S-BOX (INSECURE)"
    [ testCase "msbox: fresh mask removed inside `f_aml` (8-bit inversion)" $
        assert $ predMSBroken
    ]
  ]


---------------------
--- Test vectors. ---
---------------------

s = Just . S.singleton
e = Just S.empty

am2   = mp Dat $ (s 0) ~> (s 1) ~> (pure Nil)
m2    = mp Msk $ (s 0) ~> (s 1) ~> (pure Nil)
am2'  = mp Dat $ (s 3) ~> (s 4) ~> (pure Nil)
m2'   = mp Msk $ (s 3) ~> (s 4) ~> (pure Nil)

am4  = mp Dat $ (s 0) ~> (s 1) ~> (s 3) ~> (s 4) ~> (pure Nil)
m4   = mp Msk $ (s 0) ~> (s 1) ~> (s 3) ~> (s 4) ~> (pure Nil)
am4' = mp Dat $ (s 5) ~> (s 6) ~> (s 7) ~> (s 8) ~> (pure Nil)
m4'  = mp Msk $ (s 5) ~> (s 6) ~> (s 7) ~> (s 8) ~> (pure Nil)

--res1  = sq2 am2      -- OK.
--res1' = am2 .*. am2  -- Wrong? It should be OK!? False positive?
--res2  = am2 .*. am2' -- OK.
--res3  = m2  .*. am2  -- Not secure!



----------------------------
--- Symbolic Predicates. ---
----------------------------

-- | Masked value.
am = am4 ++ am4'
-- | Mask.
m  = m4 ++ m4'
-- | Fresh mask (independent to the mask)
m_fresh = mp Msk $ (s 9) ~> (s 10) ~> (s 11) ~> (s 12) ~> (pure Nil)


-- | Masked inversion with fresh mask.
predMskdInv = mvalid $ m_invGF (m, m_fresh) am -- OK.

-- | Masked S-Box with fresh mask.
predMSBox = mvalid $ msbox (m, m_fresh) am -- OK.

-- | Badly masked S-Box (with ZEROED fresh mask).
predMSZeroed = mvalid $ msbox (m, m_fresh + m_fresh) am -- Not secure!

-- | Badly masked S-Box (BROKEN implementation).
predMSBroken = mvalid $ aff_trans . Broken.m_invGF (m, m_fresh) $ am -- Broken!

-- | Dependencies bw fresh_mask and the higher mask.
predDepHi = mvalid $ msbox (m, m4) am -- resized fresh mask -- Not Secure!

-- | Dependencies bw fresh_mask and the lower mask.
predDepLo = mvalid $ msbox (m, m4') am -- resized fresh mask -- Apparently still secure...

-- | Mask with repeating bits.
predRepMsk = mvalid $ msbox (m4 ++ m4, m_fresh) (am4 ++ am4)


--res   = aff_trans . m_invGF (m, m_fresh) $ am  -- OK.
--res'  = msbox (m, m_fresh) am                  -- OK.
--res'' = msbox (m, m_fresh ..^ m_fresh) am      -- Not secure!
--bres  = aff_trans . Broken.m_invGF (m, m_fresh) $ am  -- Broken!

--rdepH  = msbox (m, m4) am -- resized fresh mask -- Not Secure!
--rdepL  = msbox (m, m4') am -- resized fresh mask
