{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.CLaSH.EvalMasked where

import CLaSH.Prelude
import GHC.TypeLits

import Backend.CLaSH.Operators
import Language.Operators
import Primitives.MaskedSBOX
--import Backend.CLaSH.Eval.EvalMaskedConst


-----------------------------------------------
-- CLaSH Backend: Syntesizable IP, Testbench --
-----------------------------------------------

{-# ANN topEntity
  (defTop
    { t_name    = "MaskedSBOX"
    , t_inputs  = ["mask", "m_fresh", "m_data"]
    , t_outputs = ["m_out"]
    }) #-}
topEntity :: Signal (Unsigned 8) -- mask
          -> Signal (Unsigned 4) -- m_fresh
          -> Signal (Unsigned 8) -- masked data
          -> Signal (Unsigned 8)
topEntity m m_fresh x = f <$> m <*> m_fresh <*> x
  where
    f qm qm_fresh qx = unHW $ msbox (HW qm, HW qm_fresh) (HW qx)

{-
testInput :: Signal (Unsigned 8)
testInput = stimuliGenerator $(v inVector)

-- USAGE: sampleN 256 $ expectedOutput (topEntity testInput)
expectedOutput :: Signal (Unsigned 8) -> Signal Bool
expectedOutput = outputVerifier $(v outVector)
-}

-- checkFunctionality = and (Prelude.map not (sampleN 256 $ expectedOutput (topEntity testInput)))