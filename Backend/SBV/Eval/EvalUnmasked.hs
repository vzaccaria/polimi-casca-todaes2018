{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Backend.SBV.Eval (main) where


import Data.Bits
import Data.SBV               ((.==), SBool, SWord8, prove)
import Data.SBV.Tools.CodeGen (cgInput, cgReturn, compileToC)
import Data.Word

import qualified Data.SBV.Examples.Crypto.AES as A

import Backend.SBV.Operators
import Primitives.UnmaskedSBOX


main = testUnmasked
--main = genC

testUnmasked :: IO ()
testUnmasked = do
  testCase "Unmasked S-Box (Wolkerstorfer)" $ prove sboxCorrect

-- | Symbolic version of the unmasked S-Box by Wolkerstorfer et al.
sbox :: SWord8 -> SWord8
sbox a = unSY $ sbox' (SY a)

-- | Symbolic predicate for verifying the unmasked S-Box.
--   Equivalence check with SBV's reference S-Box implementation.
sboxCorrect :: SWord8 -> SBool
sboxCorrect a = sbox a .== A.sbox a

-- | Generate the "golden model" of the unmasked S-Box in C.
genC :: IO ()
genC = compileToC (Just "sbox") "sbox" $ do
         x <- cgInput "x"
         cgReturn $ sbox x


-- | Utility (refactor!)

testCase s t = do
  putStr $ s ++ ": "
  print =<< t