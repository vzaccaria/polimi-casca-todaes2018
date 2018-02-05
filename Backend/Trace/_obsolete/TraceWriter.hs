{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}


module Backend.Trace.TraceWriter where

import qualified CLaSH.Prelude as C ((!), high, pack, resize, shiftL, unpack)
import CLaSH.Prelude                ((++#), BitVector, Unsigned)
import Control.Monad.Writer.Strict
import Data.Bits
import qualified Data.DList    as DL
import Data.Monoid
import Prelude
import qualified Debug.Trace   as D (trace)

import Language.Operators

import Primitives.Math
import Primitives.Math22
import Primitives.UnmaskedSBOX (sbox')


{-
(!) w1 nn = do
  x <- w1
  return $ x C.! nn

unpack w = do
  x <- w
  return $ C.unpack x
-}


type Probe = [String]
--type Probe = DL.DList String

data family Trace a
data instance Trace U8 = T8 { unT8 :: (Writer Probe (Unsigned  8)) } deriving (Show)
data instance Trace U4 = T4 { unT4 :: (Writer Probe (Unsigned  4)) } deriving (Show)
data instance Trace U2 = T2 { unT2 :: (Writer Probe (Unsigned  2)) } deriving (Show)
data instance Trace U1 = T1 { unT1 :: (Writer Probe (BitVector 1)) } deriving (Show)


instance Symantics Trace where
  literal2 = T2 . return . fromInteger
  literal4 = T4 . return . fromInteger

  --n (T1 x) = T1 $ x >>= \v -> return $ S1 (complement (unS1 v))
  n (T1 x)           = T1 $ x >>= return . complement

  (T1 a) .&  (T1 b)  = T1 $ liftBin (.&.) a b
  (T1 a) .^  (T1 b)  = T1 $ liftBin (xor) a b
  (T2 a) .^. (T2 b)  = T2 $ liftBin (xor) a b
  (T4 a) ..^ (T4 b)  = T4 $ liftBin (xor) a b -- TODO: also probe here.

  (T2 a) .! nn       = T1 $ a >>= return . C.unpack . (flip (C.!)) nn
  (T4 a) ..! nn      = T1 $ a >>= return . C.unpack . (flip (C.!)) nn
  (T8 a) ...! nn     = T1 $ a >>= return . C.unpack . (flip (C.!)) nn

  pack2 (T1 w1) (T1 w2) = T2 w
    where
      w = do
        x1 <- w1
        let x2 = fst . runWriter $ w2
        combineLog w1 w2
        let x1' = C.pack x1
        let x2' = C.pack x2
        return $ C.unpack $ x1' ++# x2'

  -- | Applying probes at `pack4` level.
  pack4 (T1 w1) (T1 w2) (T1 w3) (T1 w4) = T4 w
    where
      w = do
        x1 <- w1
        --combineLog w1 w2 >>= (flip combineLog) w3 >>= (flip combineLog) w4
        let x2 = fst . runWriter $ w2
        let x3 = fst . runWriter $ w3
        let x4 = fst . runWriter $ w4
        combo4 w1 w2 w3 w4
        let x1' = C.pack x1
        let x2' = C.pack x2
        let x3' = C.pack x3
        let x4' = C.pack x4
        let o   = C.unpack $ x1' ++# x2' ++# x3' ++# x4'
        ---------------------------------------
        -- Probing here. ----------------------
        let msg = show4 o
        tell $ [msg]                  -- FIX: it explodes when doing multiple (..!) ops after a `tell`!!!
        --tell $ DL.singleton $ msg
        D.trace ("D.trace: " ++ msg) $ return o
        ---------------------------------------
        ---------------------------------------


  pack8 (T4 w1) (T4 w2) = T8 w
    where
      w = do
        a1 <- w1
        let a2 = fst . runWriter $ w2
        combineLog w1 w2
        return $ (C.resize a1 `shiftL` 4) .|. C.resize a2



-----------------------
-- Data constructors --
-----------------------

t8 :: Unsigned 8 -> Trace U8
t8 x = T8 $ writer (x, [])
--t8 x = T8 $ writer (x, DL.empty)

t4 :: Unsigned 4 -> Trace U4
t4 x = T4 $ writer (x, [])
--t4 x = T4 $ writer (x, DL.empty)

t2 :: Unsigned 2 -> Trace U2
t2 x = T2 $ writer (x, [])
--t2 x = T2 $ writer (x, DL.empty)

t1 :: BitVector 1 -> Trace U1
t1 x = T1 $ writer (x, [])
--t1 x = T1 $ writer (x, DL.empty)



---------------
-- Utilities --
---------------


--liftBin :: (Monoid p) => (a -> a -> b) -> Writer p a -> Writer p b -> Writer p b
liftBin f w1 w2 = do
  x <- w1
  let y = fst . runWriter $ w2
  combineLog w1 w2
  return $ f x y


dropPrefix x y = drop (length $ takeWhile (\(u,v) -> u == v) z) y
  where z = zip x y

combineLog :: (Eq p, MonadWriter [p] m) => Writer [p] a -> Writer [p] b -> m ()
combineLog w1 w2 = do
  let x = execWriter $ w1
  let y = execWriter $ w2
  tell $ dropPrefix x y  -- BUGGED for complex datapaths!

combo4 w1 w2 w3 w4 =
  combineLog (combineLog (combineLog w1 w2) w3) w4

-- TODO: use `testBit` for computing the HW.
show4 :: Unsigned 4 -> String
show4 x = (show x1) ++ (show x2) ++ (show x3) ++ (show x4)
  where
    x1 = x C.! 0
    x2 = x C.! 1
    x3 = x C.! 2
    x4 = x C.! 3

--applyProbe :: Trace U4 -> Writer [String] (Trace U4)
--applyProbe x = writer (x, ["Got"])

{-
combineLog4 :: (Eq p, MonadWriter [p] m) => Writer [p] a -> Writer [p] b -> Writer [p] c -> Writer [p] d -> m ()
combineLog4 w1 w2 w3 w4 = do
  let a = execWriter $ w1
  let b = execWriter $ w2
  let c = execWriter $ w3
  let d = execWriter $ w4

  let z12 = dropPrefix a b
  --let w12 = combineLog w1 w2
  tell a
  ...
-}


-----------
-- Tests --
-----------

t1_uno  = t1 C.high
t1_zero = n t1_uno
t8_zero = t8 0
t8_res  = pack8 (literal4 2) (literal4 10)
res     = fst . runWriter . unT8 $ t8_res

-- λ gfMapInv22 (t2 1, t2 1)
-- >> OK.
-- λ inv4 $ inv4 $ gfMapInv22 (t2 1, t2 1)
-- >> OK.
-- λ inv4 $ inv4 $ inv4 $ inv4 $ gfMapInv22 (t2 1, t2 1)
-- >> OK.
-- λ inv4 $ inv4 $ (..*) (t4 1) (t4 2)
-- >> OK.
-- λ inv8 (inv4 $ inv4 $ inv4 $ (..*) (t4 1) (t4 2), t4 2)
-- >> OK :-)


-- λ gfMapInv $ inv8 (inv4 $ inv4 $ inv4 $ (..*) (t4 1) (t4 2), t4 2)
-- BROKEN: `combineLog`/`dropPrefix` do not work in this case.
-- λ sbox' $ t8 0
-- BROKEN (more `D.trace`s than probes)
-- λ sbox' $ t8 1
-- BROKEN (MUCH more probes than `D.trace`s!)
