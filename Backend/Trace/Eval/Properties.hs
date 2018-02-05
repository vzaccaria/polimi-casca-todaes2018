{-# LANGUAGE DataKinds #-}

module Backend.Trace.Eval.Properties 
  ( -- * Properties
    testNOMASKS, testBrokenOsw
--, testDepMasks
  , testWithFreshM, testNoFreshM
  , testDepMaskLO, testDepMaskHI
  )
where


import Data.Bits
import CLaSH.Prelude (Unsigned)

import qualified CLaSH.Prelude as C

import Backend.Trace.Operators
import Language.Operators
import Primitives.Math22
import Primitives.Math         (aff_trans)
import Primitives.UnmaskedSBOX (sbox')
import Primitives.MaskedSBOX   (msbox)

import qualified Primitives.Broken.MaskedGFInv as Broken (m_invGF)


-- | Properties |--------------------------------------------------------------

testNOMASKS :: (Trace 8, Trace 4, Trace 8) -> Trace 8
--testNOMASKS    (_, _ , x) = msbox (tr8 0, tr4 0) x
testNOMASKS (TR mu, _ , TR xu) =
  let y = tr8 $ xu `xor` mu -- unmasked input
  in  msbox (tr8 0, tr4 0) y

testWithFreshM :: (Trace 8, Trace 4, Trace 8) -> Trace 8
testWithFreshM (m, mf, x) = msbox (m, mf) x

testNoFreshM   :: (Trace 8, Trace 4, Trace 8) -> Trace 8
testNoFreshM   (m, _ , x) = msbox (m, tr4 0) x

testBrokenOsw  :: (Trace 8, Trace 4, Trace 8) -> Trace 8
testBrokenOsw  (m, mf, x) = aff_trans . Broken.m_invGF (m, mf) $ x

--testDepMasks   :: (Trace 8, Trace 4, Trace 8) -> Trace 8
--testDepMasks   (TR mu, mf, x) =
--  let y = tr8 $ mu .&. (C.resize (unTR mf)) -- new mask := mask AND fresh_mask
--  --let y = tr8 $ mu `xor` (C.resize (unTR mf)) -- new mask := mask XOR fresh_mask  
--  in  msbox (y, mf) x

testDepMaskLO  :: (Trace 8, Trace 4, Trace 8) -> Trace 8
testDepMaskLO  (m, _, x) =
  let y = tr4 $ C.resize (unTR m) -- new fresh_mask := l.s.4bits of mask
  in  msbox (m, y) x

testDepMaskHI  :: (Trace 8, Trace 4, Trace 8) -> Trace 8
testDepMaskHI  (m, _, x) =
  let 
    y   = tr4 $ C.resize $ C.shiftR ((unTR m) .&. mHI) 4  -- new fresh_mask := f.s.4bits of mask
    mHI = 240 :: Unsigned 8
  in
    msbox (m, y) x
