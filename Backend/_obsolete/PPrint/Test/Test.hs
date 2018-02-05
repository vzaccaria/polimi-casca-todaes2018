{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Backend.PPrint.Test where

import Language.Operators
import Backend.PPrint.Operators
import Primitives.UnmaskedSBOX

import Data.Word
import Data.Bits

inp1 :: PPrint U8
inp1 = P "input"

sboxPPrint :: String
sboxPPrint = unP (sbox' inp1)


