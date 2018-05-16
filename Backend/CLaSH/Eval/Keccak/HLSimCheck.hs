{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.CLaSH.Keccak.HLSimCheck where

import CLaSH.Prelude hiding ((+), assert, map, zipWith)
import Prelude hiding ((!!), (+))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Language.Operators
import Backend.CLaSH.Operators
import Backend.CLaSH.Eval.Keccak.EvalKeccak

--------------------------------------------
--------------- Unit tests. ----------------
--------------------------------------------
main = defaultMain checkFunctionality

checkFunctionality :: TestTree
checkFunctionality =
    testGroup
        "Test Keccak"
        [ testGroup
              "HLSim"
              [ testCase "HL/SBV correspondence check" $
                assert $ hwChi3 (26, 25, 14) == (13, 25, 16)]]