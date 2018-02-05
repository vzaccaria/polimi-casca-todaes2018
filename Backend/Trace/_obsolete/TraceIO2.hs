{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}


module Backend.Trace.TraceIO2 where

import qualified CLaSH.Prelude as C (high, pack, resize, shiftL, unpack)
import CLaSH.Prelude                ((!), (++#), BitVector, Unsigned)
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Prelude

import Language.Operators

import qualified Debug.Trace   as D (trace)
import Primitives.Math
import Primitives.Math22
import Primitives.UnmaskedSBOX (sbox')


instance Show (IO ()) where
  show _ = "<io>"


data family Trace a
data instance Trace U8 = T8 (Unsigned  8) (IO ()) deriving (Show)
data instance Trace U4 = T4 (Unsigned  4) (IO ()) deriving (Show)
data instance Trace U2 = T2 (Unsigned  2) (IO ()) deriving (Show)
data instance Trace U1 = T1 (BitVector 1) (IO ()) deriving (Show)


instance Symantics Trace where
  literal2 n = T2 (fromInteger n) (return ())
  literal4 n = T4 (fromInteger n) (return ())

  n (T1 x io)           = T1 (complement x) io

  (T1 a io1) .&  (T1 b io2) = T1 (a .&. b)   $ io1 >> io2
  (T1 a io1) .^  (T1 b io2) = T1 (a `xor` b) $ io1 >> io2
  (T2 a io1) .^. (T2 b io2) = T2 (a `xor` b) $ io1 >> io2
  (T4 a io1) ..^ (T4 b io2) = T4 (a `xor` b) $ io1 >> io2 >> print "ciao" -- Fake probe

  (T2 a io) .! nn        = T1 (C.unpack $ a ! nn) io
  (T4 a io) ..! nn       = T1 (C.unpack $ a ! nn) io
  (T8 a io) ...! nn      = T1 (C.unpack $ a ! nn) io

  pack2 (T1 x1 io1) (T1 x2 io2) =
    let
      x1' = C.pack x1
      x2' = C.pack x2
      p = C.unpack $ x1' ++# x2'
    in
      T2 p $ io1 >> io2

  pack4 (T1 x1 io1) (T1 x2 io2) (T1 x3 io3) (T1 x4 io4) =
    let
      x1' = C.pack x1
      x2' = C.pack x2
      x3' = C.pack x3
      x4' = C.pack x4
      p = C.unpack $ x1' ++# x2' ++# x3' ++# x4'
    in
      T4 p $ io1 >> io2 >> io3 >> io4

  pack8 (T4 x1 io1) (T4 x2 io2) =
    let p = (C.resize x1 `shiftL` 4) .|. C.resize x2
    in  T8 p $ io1 >> io2

-------------------
-- AUX Functions --
-------------------

show4 :: Unsigned 4 -> String
show4 x = (show x1) ++ (show x2) ++ (show x3) ++ (show x4) ++ " "
  where
    x1 = x ! 0
    x2 = x ! 1
    x3 = x ! 2
    x4 = x ! 3



--------------------------
-- Problematic Use Case --
--------------------------

-- λ (\(T4 _ x) -> x) $ inv4 $ (literal4 3) ..^ (literal4 9)
-- >> This prints A LOT of "probes"!
