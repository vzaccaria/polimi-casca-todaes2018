{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}


module Backend.Trace.Graph2 where

import Prelude
import qualified CLaSH.Prelude as C   (high, pack, resize, shiftL, unpack)
import CLaSH.Prelude                  ((!),(++#), BitVector, Unsigned)
import Data.Bits
import GHC.TypeLits                   (KnownNat)
import qualified Data.DList    as L
import qualified Data.Map      as Map
import qualified Data.Set      as Set (Set, empty, fromList, singleton, union)
import qualified Debug.Trace   as D   (trace)

import Language.Operators

import Primitives.Math
import Primitives.Math22
import Primitives.UnmaskedSBOX (sbox')



------------------
--- Data Types ---
------------------

data Probe = NoP | P String deriving (Show)

--data DAG = Empty
--         | Node Probe [DAG] 
--         deriving (Show)

data Op = L2 | L4 -- | IN
        | A  | N
        | X  | X2 | X4 
        | P2 | P4 | P8 
        | G2 | G4 | G8 
        deriving (Eq, Ord, Show)

newtype GKey = GKey (L.DList Op) deriving (Eq, Ord, Show)

type    GNodes = Map.Map GKey Probe
type    GArcs  = Map.Map GKey (Set.Set GKey)

data DAG = DAG
  { _ns :: GNodes
  , _as :: GArcs
  }



----------------------------------
--- Type Classes and Instances ---
----------------------------------

gcombine :: DAG -> DAG -> DAG
gcombine g1 g2 = DAG ns as
  where
    ns = Map.union (_ns g1) (_ns g2) -- OK.
    as = Map.unionWith Set.union (_as g1) (_as g2) -- OK. (not necessary, we are going bottom-up) -- it should throw an exception instead??

--gcombines :: [DAG] -> DAG
--gcombines gs = DAG ns as
--  where
--    ns = Map.union

instance Show DAG where
  show _ = "<dag>"


-- Graph as a Map:
-- Node:
--    Key   <-- (unique) ordered list of Ops!
--    Value <-- Probe
-- Arcs:
--    Key   <-- list of keys


--unprobed :: [DAG] -> DAG
--unprobed !ds@(d:_) = Node "" ds

--probed :: Probe -> [DAG] -> DAG
--probed !p !ds@(d:_) = Node p ds


data family Trace a
data instance Trace U8 = T8  (Unsigned  8)  DAG  GKey  deriving (Show) -- TODO: UNPACKED + !(,)
data instance Trace U4 = T4  (Unsigned  4)  DAG  GKey  deriving (Show)
data instance Trace U2 = T2  (Unsigned  2)  DAG  GKey  deriving (Show)
data instance Trace U1 = T1  (BitVector 1)  DAG  GKey  deriving (Show)


-----------------------
-- Utility functions --
-----------------------

oneOp :: Op -> Bool
oneOp = (flip elem) [N, G2, G4, G8]

binOp :: Op -> Bool
binOp = (flip elem) [A, X, X2, X4, P2, P8]


newNode1 :: Op -> GKey -> (GKey, DAG)
newNode1 op k@(GKey ks)
  | oneOp op = (k', dag)
    where
      k'  = GKey $ ks `L.snoc` op
      dag = DAG n a
      n = Map.singleton k' NoP -- new node, no probes.
      a = Map.singleton k' $ Set.singleton k -- parent node in the list.


newNode2 :: Op -> GKey -> GKey -> (GKey, DAG)
newNode2 op k1@(GKey ks1) k2@(GKey ks2)
  | binOp op = (k', dag)
    where
      k'  = GKey $ (ks1 `L.snoc` op) `L.append` ks2
      dag = DAG n a
      n = Map.singleton k' NoP -- new node, no probes.
      a = Map.singleton k' $ Set.fromList [k1,k2] -- two parent nodes in the list.


newNode4 :: Op -> GKey -> GKey -> GKey -> GKey -> (GKey, DAG)
newNode4 op@P4 k1@(GKey ks1) k2@(GKey ks2) k3@(GKey ks3) k4@(GKey ks4) =
  let
    k' = GKey $ ks1 `L.append` ks2 `L.append` ks3 `L.append` (ks4 `L.snoc` op)
    dag = DAG n a
    n = Map.singleton k' NoP -- new node, no probes.
    a = Map.singleton k' $ Set.fromList [k1,k2,k3,k4] -- four parent nodes in the list.
  in
    (k', dag)


dagLeaf :: Op -> (GKey, DAG)
dagLeaf op = (k, DAG ns as)
  where
    ns = Map.singleton k NoP -- No probes.
    as = Map.singleton k Set.empty
    k  = (GKey $ L.singleton op) -- TODO: implement GKey singleton?

dagIn :: (GKey, DAG)
--dagIn = dagLeaf IN
dagIn = (GKey L.empty, DAG n a)
  where
    n = Map.empty
    a = Map.empty


in1 x = let (k,g) = dagIn in T1 x g k 
in2 x = let (k,g) = dagIn in T2 x g k
in4 x = let (k,g) = dagIn in T4 x g k
in8 x = let (k,g) = dagIn in T8 x g k

--- ~~~ ---

--dagSingleton :: GKey -> DAG
--dagSingleton k = dagSingletonProbed k NoP

--dagSingletonProbed :: GKey -> Probe -> DAG
--dagSingletonProbed k p = DAG ns as
--  where
--    ns = undefined -- Map.singleton k p
--    as = undefined -- Map.singleton k 


-----------------------
-- Symantics Instance --
-----------------------

instance Symantics Trace where
  literal2 x =
    let (k, dag) = dagLeaf L2
    in  T2 (fromInteger x) dag k

  literal4 x =
    let (k, dag) = dagLeaf L4
    in  T4 (fromInteger x) dag k


  n (T1 x g k) = -- D.trace "probe" $ T1 (complement x) $ probed "probe" [d]
    let
      (k', node') = newNode1 N k      -- new node, with arc to the parent.
      g'          = gcombine g node'  -- new DAG
    in
      T1 (complement x) g' k'
 

  (T1 x g1 k1) .&  (T1 y g2 k2)  =
    let
      (k', node') = newNode2 A k1 k2                -- new node, with arc to the parents.
      g'          = gcombine (gcombine g1 g2) node' -- TODO: write `gcombines`
    in
      T1 (x  .&.  y) g' k'

  (T1 x g1 k1) .^  (T1 y g2 k2)  =
    let
      (k', node') = newNode2 X k1 k2
      g'          = gcombine (gcombine g1 g2) node' -- TODO: write `gcombines`
    in
       T1 (x `xor` y) g' k'

  (T2 x g1 k1) .^. (T2 y g2 k2)  =
    let
      (k', node') = newNode2 X2 k1 k2
      g'          = gcombine (gcombine g1 g2) node' -- TODO: write `gcombines`
    in
      T2 (x `xor` y) g' k'

  (T4 x g1 k1) ..^ (T4 y g2 k2)  =
    let
      (k', node') = newNode2 X4 k1 k2
      g'          = gcombine (gcombine g1 g2) node' -- TODO: write `gcombines`
    in
      T4 (x `xor` y) g' k'

{-
  -- WRONG: EACH BIT MUST BE MARKED DIFFERENTLY...

  (T2 x g k)   .! nn  =  T1 (C.unpack $ x ! nn) g' k'
    where
      (k', node') = newNode1 G2 k
      g'          = gcombine g node'
  
  (T4 x g k)  ..! nn  = T1 (C.unpack $ x ! nn) g' k'
    where
      (k', node') = newNode1 G4 k
      g'          = gcombine g node'

  (T8 x g k) ...! nn  = T1 (C.unpack $ x ! nn) g' k'
    where
      (k', node') = newNode1 G8 k
      g'          = gcombine g node'
-}

  pack2 (T1 x1 g1 k1) (T1 x2 g2 k2) =
    let 
      x1' = C.pack x1
      x2' = C.pack x2
      (k', node') = newNode2 P2 k1 k2
      g'          = gcombine (gcombine g1 g2) node' -- TODO: write `gcombines`
    in
      T2 (C.unpack $ x1' ++# x2') g' k'

  -- | Applying probes on negations.
  pack4 (T1 x1 g1 k1) (T1 x2 g2 k2) (T1 x3 g3 k3) (T1 x4 g4 k4) =
    let
      x1' = C.pack x1
      x2' = C.pack x2
      x3' = C.pack x3
      x4' = C.pack x4
      y   = C.unpack $ x1' ++# x2' ++# x3' ++# x4'
      (k', node') = newNode4 P4 k1 k2 k3 k4
      g'          = gcombine (gcombine (gcombine (gcombine g1 g2) g3) g4) node' -- TODO: write `gcombines`
    in
       T4 y g' k'

  pack8 (T4 w1 g1 k1) (T4 w2 g2 k2) =
    let
      w = (C.resize w1 `shiftL` 4) .|. C.resize w2
      (k', node') = newNode2 P8 k1 k2
      g'          = gcombine (gcombine g1 g2) node' -- TODO: write `gcombines`
    in
      T8 w g' k'



-----------
-- Prove --
-----------

--ok = aff_trans (T8 42 Empty)
--ko = aff_trans . aff_trans $ (T8 42 Empty) -- nodes are being duplicated...


--show4 :: Unsigned 4 -> String
--show4 x = (show x1) ++ (show x2) ++ (show x3) ++ (show x4) ++ " "
--  where
--    x1 = x ! 0
--    x2 = x ! 1
--    x3 = x ! 2
--    x4 = x ! 3
