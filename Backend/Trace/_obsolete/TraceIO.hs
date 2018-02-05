{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}


module Backend.Trace.TraceIO where

import qualified CLaSH.Prelude as C ((!), high, pack, resize, shiftL, unpack)
import CLaSH.Prelude                ((++#), BitVector, Unsigned)
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Prelude

import Language.Operators

import qualified Debug.Trace   as D (trace)


import Primitives.Math
import Primitives.Math22
import Primitives.UnmaskedSBOX (sbox')


--type Probe = [String]

data family Trace a
data instance Trace U8 = T8 { unT8 :: IO (Unsigned  8) }
data instance Trace U4 = T4 { unT4 :: IO (Unsigned  4) }
data instance Trace U2 = T2 { unT2 :: IO (Unsigned  2) }
data instance Trace U1 = T1 { unT1 :: IO (BitVector 1) }


--instance (KnownNat n) => Show (IO (Unsigned n)) where
--  show _ = "<unT>"

--instance (KnownNat n) => Show (IO (BitVector n)) where
--  show _ = "<unT>"


instance Symantics Trace where
  literal2 = T2 . return . fromInteger
  literal4 = T4 . return . fromInteger

  n (T1 x)           = T1 $ x >>= return . complement

  (T1 a) .&  (T1 b)  = T1 $ liftM2 (.&.) a b
  (T1 a) .^  (T1 b)  = T1 $ liftM2 (xor) a b
  (T2 a) .^. (T2 b)  = T2 $ liftM2 (xor) a b
  (T4 a) ..^ (T4 b)  = T4 $ liftM2 (xor) a b -- >>= (\x -> putStr (show4 x) >> return x) -- TODO: also probe here

  (T2 a) .! nn       = T1 $ a >>= return . C.unpack . (flip (C.!)) nn
  (T4 a) ..! nn      = T1 $ a >>= return . C.unpack . (flip (C.!)) nn
  (T8 a) ...! nn     = T1 $ a >>= return . C.unpack . (flip (C.!)) nn

  pack2 (T1 w1) (T1 w2) = T2 w
    where
      w = do
        x1 <- w1
        x2 <- w2
        let x1' = C.pack x1
        let x2' = C.pack x2
        return $ C.unpack $ x1' ++# x2'


  -- | Applying probes at `pack4` level.

  pack4 (T1 w1) (T1 w2) (T1 w3) (T1 w4) = T4 w
    where
      w = do
        x1 <- w1
        x2 <- w2
        x3 <- w3
        x4 <- w4
        let x1' = C.pack x1
        let x2' = C.pack x2
        let x3' = C.pack x3
        let x4' = C.pack x4
        let o   = C.unpack $ x1' ++# x2' ++# x3' ++# x4'
        ---------------------------------------
        -- Probing here. ----------------------
        let msg = show4 o
        putStrLn $ "Probe: " ++ msg
        D.trace ("D.trace: " ++ msg) $ return o
        ---------------------------------------
        ---------------------------------------


  pack8 (T4 w1) (T4 w2) = T8 w
    where
      w = do
        a1 <- w1
        a2 <- w2
        return $ (C.resize a1 `shiftL` 4) .|. C.resize a2



---------------
-- Utilities --
---------------


t8 :: Unsigned 8 -> Trace U8
t8 x = T8 $ return x

t4 :: Unsigned 4 -> Trace U4
t4 x = T4 $ return x

t2 :: Unsigned 2 -> Trace U2
t2 x = T2 $ return x

t1 :: BitVector 1 -> Trace U1
t1 x = T1 $ return x

{-
(!) w1 nn = do
  x <- w1
  return $ x C.! nn

unpack w = do
  x <- w
  return $ C.unpack x
-}

show4 :: Unsigned 4 -> String
show4 x = (show x1) ++ (show x2) ++ (show x3) ++ (show x4) ++ " "
  where
    x1 = x C.! 0
    x2 = x C.! 1
    x3 = x C.! 2
    x4 = x C.! 3



-----------
-- Tests --
-----------

t1_uno   = t1 C.high
t1_zero  = n t1_uno
t8_zero  = t8 0

-- λ unT4 $ gfMapInv22 (t2 1, literal2 1)
   -- >> OK.
-- λ unT4 $ inv4 (t4 15)
   -- >> OK.
-- λ unT4 $ inv4 $ gfMapInv22 (t2 2, literal2 1)
   -- >> BROKEN!!!
