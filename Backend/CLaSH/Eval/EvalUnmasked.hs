{-# LANGUAGE DataKinds #-}

module Backend.CLaSH.Eval (topEntity) where


import CLaSH.Prelude
import Language.Operators

import Backend.CLaSH.Operators
import Primitives.UnmaskedSBOX


topEntity :: Signal (Unsigned 8) -> Signal (Unsigned 8)
topEntity x =  myf <$> x where
  myf q = unHW $ sbox' (HW q)
