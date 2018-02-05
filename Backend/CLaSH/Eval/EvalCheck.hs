{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.CLaSH.EvalCheck (main) where


import CLaSH.Prelude hiding ((+), assert, map, zipWith)
import Prelude       hiding ((!!), (+))

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Language.Operators
import Backend.CLaSH.Operators
import Primitives.Math
import Primitives.Math22
import Primitives.UnmaskedSBOX
import Primitives.MaskedGFInv
import Primitives.MaskedSBOX

import qualified Primitives.WolkerstorferGFInvConst as WS (inVector, outVector)
import qualified Primitives.Broken.MaskedGFInv      as B  (m_invGF)


--------------------------
--- Renaming/Utilities ---
--------------------------

inVector  = map HW WS.inVector
outVector = map HW WS.outVector
set16  = map HW [0,1..15]
set16' = map HW [1..15]
set4   = map HW [0,1,2,3]
set4'  = map HW [1,2,3]


--------------------------------------------
--------------- Unit tests. ----------------
--------------------------------------------

main = defaultMain checkFunctionality

checkFunctionality :: TestTree
checkFunctionality = testGroup "Test Polymorphic Primitives" [
  testGroup "Basic properties"
    [ testCase "GF((2²)²) mappings" $ assert $
        all (\x -> (gfMapInv22 . gfMap22 $ x) == x) set16
    , testCase "mul2" $ assert $
        all (\x -> (mul2 (inv2 x) x) == hw2 1) set4' -- | inv2 CANNOT INVERT ZERO !
    , testCase "inv2" $ assert $
        all (\x -> (inv2 . inv2 $ x) == x) set4
    , testCase "mul4" $ assert $
        all (\x -> (mul4 (inv4 x) x) == hw4 1) set16' -- | inv4 CANNOT INVERT ZERO !
    , testCase "sq4" $ assert $
        all (\x -> (sq4 x) == (mul4 x x)) set16
    , testCase "inv4" $ assert $
        all (\x -> (inv4 . inv4 $ x) == x) set16
    ]
  ,
  testGroup "Basic properties (QC)"
    [ testProperty "inv2" $ \x -> (inv2 . inv2 $ HW x) == HW x
    , testProperty "inv4" $ \x -> (inv4 . inv4 $ HW x) == HW x
    , testProperty "GF((2²)²) mappings" $ \x -> (gfMapInv22 . gfMap22 $ HW x) == HW x
    ]
  ,
  testGroup "Unmasked S-Box" 
    [ testCase "Tower field S-Box" $ assert $
        and $ zipWith (\x y -> (sbox' x) == y) inVector outVector
    ]
  ,
  testGroup "Masked S-Box (QC)"
    [ testProperty "Masked S-Box (zeroed masks)" checkZeroed
    , testProperty "Masked S-Box (only fresh_mask)" checkFreshMask
    , testProperty "Masked S-Box (both masks)" checkBothMasks
    , testProperty "Masked inversion" checkInv
    --,
    --  testCase "Masked SBOX (zeroed masks)" $ assert $ 
    --    and $ map (checkMSBox (0,0)) inVector
    ]
  ,
  testGroup "Incorrectly masked inversion (QC)"
    [ testProperty "Functional equivalence check" checkBrokenEq
    ]
  ]


-- | QC test functions.

checkZeroed am = checkMSBox (0,0) am
checkFreshMask mM am = checkMSBox (0,mM) am
checkBothMasks = checkMSBox


-- | Functional equivalence between `m_invGF` and `B.m_invGF`.

checkBrokenEq :: (Unsigned 8, Unsigned 4) -> Unsigned 8 -> Bool
checkBrokenEq (m, mM) am = let
    msk' = (HW m, HW mM)
    am'  = HW am
  in (m_invGF msk' am') == (B.m_invGF msk' am')


-- | Functional equivalence between `sbox'` and `msbox`.

checkMSBox :: (Unsigned 8, Unsigned 4) -> Unsigned 8 -> Bool
checkMSBox (m, mM) am = let
    a = am `xor` m
    unmskd = aff_trans $ (invGF $ HW a) + (HW m)
    mskd   = msbox (HW m, HW mM) (HW am)
  in
    unmskd == mskd 
    --(sbox' a) `xor` m  ==  msbox msk am


-- | Functional equivalence between `invGF` and `m_invGF`.
checkInv :: (Unsigned 8, Unsigned 4) -> Unsigned 8 -> Bool
checkInv (m, mM) am = let
    a = am `xor` m
    unmskd = (invGF $ HW a) + (HW m)
    mskd   = m_invGF (HW m, HW mM) (HW am)
  in
    unmskd == mskd
