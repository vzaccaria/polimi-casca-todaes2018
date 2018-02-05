{-# LANGUAGE DataKinds        #-}
-- {-# LANGUAGE FlexibleContexts #-}

module Primitives.Todaes 
  (fAdder, hAdder)
where

import Prelude  hiding ((+), (*), (++), sum)
import Data.SBV ((|||), (&&&), (<=>), (.==), (.>), (.<), (.<=), (.&.), 
                 constrain, forall, prove, xor, SBool, SWord8)

import qualified Data.SBV as SBV

import Backend.SBV.Operators
import Language.Operators


main = prove predicate


-- | Half adder specification.
hAdder :: (Symantics repr) => repr 1 -> repr 1 -> (repr 1, repr 1)
hAdder a b = (sum, carry)
  where
    sum   = a + b
    carry = a * b

-- | Full adder specification.
fAdder a b cin = (sum, cout)
  where
    (s1, c1)  = hAdder a b
    (sum, c2) = hAdder cin s1
    cout = c1 + c2

-- | 4-bit adder specification.
adder4 :: (Symantics repr) => repr 4 -> repr 4 -> repr 1 -> (repr 4, repr 1)
adder4 a b cin = (sum, co3)
  where
    (s0, co0) = fAdder (a!0) (b!0) cin
    (s1, co1) = fAdder (a!1) (b!1) co0
    (s2, co2) = fAdder (a!2) (b!2) co1
    (s3, co3) = fAdder (a!3) (b!3) co2
    sum = s3 ++ s2 ++ s1 ++ s0


-- | SBV predicate for functional verification of the 4-bit adder.
predicate = do
    a   <- forall "a"
    b   <- forall "b"
    cin <- forall "cin"
    constrain $ a .<= 0xf
    constrain $ b .<= 0xf
    constrain $ cin .== 0x0
    return $ checkAdder4 a b cin -- (checkAdder4Sum a b cin) &&& (checkAdder4Carry a b cin)

checkAdder4 :: SWord8 -> SWord8 -> SWord8 -> SBool
checkAdder4 a b cin = let
    dsl = adder4 (sy a) (sy b) (sy cin)
    sbv = snd $ SBV.fullAdder a b
  in
    -- prove sum
    (0xf .&. sbv) .== (unSY . fst $ dsl)
    &&&
    -- prove carry
    (0xf .< sbv)  <=> ((unSY . snd $ dsl) .== 0x1)


--checkAdder4Sum :: SWord8 -> SWord8 -> SWord8 -> SBool
--checkAdder4Sum a b cin = let
--    dsl = adder4 (sy4 a) (sy4 b) (sy1 cin)
--    sbv = SBV.fullAdder a b
--  in
--    (unSY . fst $  dsl) .== (0xf .&. snd sbv)

--checkAdder4Carry :: SWord8 -> SWord8 -> SWord8 -> SBool
--checkAdder4Carry a b cin = let
--    dsl = adder4 (sy4 a) (sy4 b) (sy1 cin)
--    sbv = SBV.fullAdder a b
--  in
--    ((unSY . snd $ dsl) .== 0x1) <=> (0xf .< snd sbv)
