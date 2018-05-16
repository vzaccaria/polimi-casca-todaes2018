{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.CLaSH.Eval.Keccak.EvalKeccak where

import qualified CLaSH.Prelude as CP
import GHC.TypeLits
import Backend.CLaSH.Operators
import Language.Operators
import Primitives.Papers.Keccak

type Row = CP.Unsigned 5

type Shares = CP.Vec 320 (Row, Row, Row)

-----------------------------------------------
-- CLaSH Backend: Syntesizable IP, Testbench --
-----------------------------------------------
hwChi3
    :: (Row, Row, Row) -> (Row, Row, Row)
hwChi3 (a,b,c) =
    let (x,y,z) = chi3 (HW a, HW b, HW c)
    in (unHW x, unHW y, unHW z)

topEntity :: CP.Signal Shares -> CP.Signal Shares
topEntity s = (CP.map hwChi3) CP.<$> s