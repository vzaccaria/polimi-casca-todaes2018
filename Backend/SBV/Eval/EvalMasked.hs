{-# LANGUAGE OverloadedStrings #-}

module Backend.SBV.EvalMasked (main) where


import Prelude  hiding ((+))
import Data.SBV ((|||), (.==), (./=), (.>), (.<=), constrain, 
                 forAll, forall, prove, xor, SBool, SWord8)

import qualified Data.SBV as S
import qualified Data.SBV.Examples.Crypto.AES as A

import Language.Operators         hiding ((++))
import Backend.SBV.Operators
import Primitives.MaskedGFInv  (m_invGF)
import Primitives.MaskedSBOX   (msbox)
import Primitives.UnmaskedSBOX (sbox')
import Primitives.Math22       ((.*.), gfMap22, gfMapInv22, inv2)
import Primitives.Math         ((..*), aff_trans, invGF, inv4, inv8)
--import Backend.SBV.Word4     (SWord4)

import qualified Primitives.Broken.MaskedGFInv as B (m_invGF)



-- | Symbolic models of the invariants for the 4-bit and 2-bit predicates.
type SWord4 = SWord8
type SWord2 = SWord8



-- | Unit tests |--------------------------------------------------------------


-- | Functional verification (symbolic proofs).
main = testMasked

testMasked :: IO ()
testMasked = do
  testCase "Unmasked S-Box (Wolkerstorfer)" $ prove predSbox
  testCase "gfMap22" $ prove predMap22
  testCase "inv2" $ prove predInv2
  testCase "mul2" $ prove predMul2
  testCase "inv4" $ prove predInv4
  testCase "mul4" $ prove predMul4
  testCase "Masked inversion (Oswald)" $ prove predMaskedInv
  testCase "Functional equivalence of badly masked primitive" $ prove predBrokenEq
  --testCase "Masked S-BOX (Oswald)" $ prove predMSBox



-- | Predicates |--------------------------------------------------------------


-- | Check basic math properties in GF(4).

predMap22 = do
    x <- forall "x"
    constrain $ x .<= 0xf
    return $ unSY (gfMapInv22 . gfMap22 $ sy4 x) .== x

predInv2 = do
    x <- forall "x"
    constrain $ x .<= 0x3
    return $ unSY (inv2 . inv2 $ sy2 x) .== x

predMul2 = do
    x <- forall "x"
    constrain $ x ./= 0
    constrain $ x .<= 0x3
    return $ unSY ((inv2 $ sy2 x) .*. (sy2 x)) .== 1


-- | Check basic unmasked inversion in GF(16).

predInv4 = do
    x <- forall "x"
    constrain $ x .<= 0xf
    return $ unSY (inv4 . inv4 $ sy4 x) .== x

predMul4 = do
    x <- forall "x"
    constrain $ x ./= 0
    constrain $ x .<= 0xf
    return $ unSY ((inv4 $ sy4 x) ..* (sy4 x)) .== 1


-- | Check Wolkerstorfer's unmasked S-Box.

predSbox :: SWord8 -> SBool
predSbox x = let
    sbox x = unSY $ sbox' (SY x)
  in sbox x .== A.sbox x


-- | Check Oswald's masked inversion.
--   (functional equivalence between `invGF` and `m_invGF`).

predMaskedInv = do
    am <- forall "am"
    m  <- forall "m" 
    mf <- forall "mf"   
    constrain $ mf .<= 0xf   -- fresh mask constrained to 4-bit values
    return $ checkMInv (m,mf) am

checkMInv :: (SWord8, SWord4) -> SWord8 -> SBool
checkMInv (m, mM) am = let
    am' = SY am
    m'  = SY m
    mM' = sy4 mM
    a'  = am' + m'
    unmskd = unSY $ (invGF a') + m'
    mskd   = unSY $ m_invGF (m', mM') am'
  in
    unmskd .== mskd


-- | Functional equivalence between `m_invGF` and the (badly masked) `B.m_invGF`.

predBrokenEq = do
    am <- forall "am"
    m  <- forall "m" 
    mf <- forall "mf"   
    constrain $ mf .<= 0xf   -- fresh mask constrained to 4-bit values
    return $ checkBrokenEq (m,mf) am

checkBrokenEq :: (SWord8, SWord4) -> SWord8 -> SBool
checkBrokenEq (m, mM) am = let
    msk' = (SY m, sy4 mM)
    am'  = SY am
    inv  = unSY $ (m_invGF msk' am')
    inv' = unSY $ (B.m_invGF msk' am')
  in inv .== inv'


---- | Check Oswald's masked S-Box (FIX: Predicate NOT correct...).
--predMSBox = do
--    am <- forall "am"
--    m  <- forall "m" 
--    mf <- forall "mf"
--    constrain $ mf .<= 0xf
--    return $ checkMSBox (m,mf) am
--
--checkMSBox :: (SWord8, SWord4) -> SWord8 -> SBool
--checkMSBox (m, mM) am = let
--    am' = SY8 am
--    m'  = SY8 m
--    mM' = sy4 mM
--    a'  = am' ^^ m'
--    unmskd = unSY8 $ (sbox' a') ^^ m'
--    mskd   = unSY8 $ msbox (m', mM') am'
--  in unmskd .== mskd



-- | Utilities |----------------------------------------------------------------


-- | Outputs the result of the proofs.
testCase s t = do
  putStr $ s ++ ": "
  print =<< t
