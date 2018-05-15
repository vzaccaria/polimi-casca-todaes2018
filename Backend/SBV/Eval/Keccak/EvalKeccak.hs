{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.CLaSH.Keccak.EvalKeccak where

import qualified CLaSH.Prelude as CP
import GHC.TypeLits
import Backend.SBV.Operators
import Language.Operators
import Primitives.Papers.Keccak
import Data.SBV
       ((|||), (.==), (./=), (.>), (.<=), constrain, forAll, forall,
        prove, xor, SBool, SWord8, Symbolic)

type Row = SWord8

sbvChi3 :: (Row, Row, Row) -> (Row, Row, Row)
sbvChi3 (a,b,c) =
    let (x,y,z) = chi3 (SY a, SY b, SY c)
    in (unSY x, unSY y, unSY z)

sbvChi :: SWord8 -> SWord8
sbvChi a = unSY (chir $ SY a)

correctness :: SWord8 -> SWord8 -> SWord8 -> SBool
correctness a b c =
    let (sa,sb,sc) = sbvChi3 (a, b, c)
        ss' = sbvChi (a `xor` b `xor` c)
    in (sa `xor` sb `xor` sc) .== ss'

isCorrect :: Symbolic SBool
isCorrect = do
    a <- forall "a"
    b <- forall "b"
    c <- forall "c"
    constrain $ a .<= 0x1f
    constrain $ b .<= 0x1f
    constrain $ c .<= 0x1f
    return $ correctness a b c

main = prove isCorrect