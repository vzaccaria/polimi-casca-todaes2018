{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}


module Backend.Trace.Graph where

import qualified CLaSH.Prelude as C (high, pack, resize, shiftL, unpack)
import CLaSH.Prelude                ((!),(++#), BitVector, Unsigned)
import Data.Bits
import GHC.TypeLits                 (KnownNat)
import Prelude
import qualified Debug.Trace   as D (trace)

import Language.Operators



import Primitives.Math
import Primitives.Math22
import Primitives.UnmaskedSBOX (sbox')


type Probe = String

data DAG = Empty
         | Node Probe [DAG] 
         deriving (Show)


unprobed :: [DAG] -> DAG
unprobed !ds@(d:_) = Node "" ds

probed :: Probe -> [DAG] -> DAG
probed !p !ds@(d:_) = Node p ds 

data family Trace a
data instance Trace U8 = T8 { _d8 :: (Unsigned  8), _dag8 :: DAG } deriving (Show)
data instance Trace U4 = T4 { _d4 :: (Unsigned  4), _dag4 :: DAG } deriving (Show)
data instance Trace U2 = T2 { _d2 :: (Unsigned  2), _dag2 :: DAG } deriving (Show)
data instance Trace U1 = T1 { _d1 :: (BitVector 1), _dag1 :: DAG } deriving (Show)


instance Symantics Trace where
  literal2 x = T2 (fromInteger x) Empty
  literal4 x = T4 (fromInteger x) Empty

  -- | Applying probes on negations.
  n (T1 x d) = D.trace "probe" $ T1 (complement x) $ probed "probe" [d]
 
  (T1 x d1) .&  (T1 y d2)  = T1 (x  .&.  y) $ unprobed [d1,d2]
  (T1 x d1) .^  (T1 y d2)  = T1 (x `xor` y) $ unprobed [d1,d2]
  (T2 x d1) .^. (T2 y d2)  = T2 (x `xor` y) $ unprobed [d1,d2]
  (T4 x d1) ..^ (T4 y d2)  = T4 (x `xor` y) $ unprobed [d1,d2]

  (T2 x d) .! nn    = T1 (C.unpack $ x ! nn) $ unprobed [d]
  (T4 x d) ..! nn   = T1 (C.unpack $ x ! nn) $ unprobed [d]
  (T8 x d) ...! nn  = T1 (C.unpack $ x ! nn) $ unprobed [d]


  pack2 (T1 x1 d1) (T1 x2 d2) =
    let 
      x1' = C.pack x1
      x2' = C.pack x2
      y = C.unpack $ x1' ++# x2'
    in
      T2 y $ unprobed [d1,d2]

  pack4 (T1 x1 d1) (T1 x2 d2) (T1 x3 d3) (T1 x4 d4) =
    let
      x1' = C.pack x1
      x2' = C.pack x2
      x3' = C.pack x3
      x4' = C.pack x4
      y = C.unpack $ x1' ++# x2' ++# x3' ++# x4'
    in
       T4 y $ unprobed [d1,d2,d3,d4]

  pack8 (T4 w1 d1) (T4 w2 d2) =
    let w = (C.resize w1 `shiftL` 4) .|. C.resize w2
    in  T8 w $ unprobed [d1,d2]


-----------
-- Prove --
-----------

ok = aff_trans (T8 42 Empty)
ko = aff_trans . aff_trans $ (T8 42 Empty) -- nodes are being duplicated...


--show4 :: Unsigned 4 -> String
--show4 x = (show x1) ++ (show x2) ++ (show x3) ++ (show x4) ++ " "
--  where
--    x1 = x ! 0
--    x2 = x ! 1
--    x3 = x ! 2
--    x4 = x ! 3
