{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
--{-# LANGUAGE Unsafe               #-}
--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE AllowAmbiguousTypes  #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
--{-# OPTIONS_GHC -dcore-lint #-}


module Backend.Trace.Operators
  ( -- * Data Types
    Trace (..)
    -- * Constructions
  , tr8, tr4, tr2, tr1
  )
where

import CLaSH.Prelude ((++#), BitVector, Unsigned, pack, unpack)
import Data.Bits
import Data.Proxy    (Proxy (..))
import GHC.TypeLits  (KnownNat, Nat, type (+), type (-), natVal)
import Prelude       hiding ((++))
import System.IO
--import Control.DeepSeq

import qualified CLaSH.Prelude as C

import Backend.Trace.Probe     (probe, showHW)
import Language.Operators



-- | Concrete Data Type |-------------------------------------------------------

data Trace (n :: Nat) = TR { unTR :: Unsigned n } deriving (Show)



-- | Class Instance |-----------------------------------------------------------

instance Symantics Trace where
  literal = TR . fromInteger
  (TR a) * (TR b) = TR $ a .&. b
  (TR a) + (TR b) = q
    where
      res = a `xor` b
      n   = natVal q
      q   = case n of
            4 -> probe (showHW res) $ TR res
            _ -> TR res

  neg (TR x) = TR . complement $ x
  (TR x) ! i = TR . unpack $ x C.! i

  (++) :: forall n m . (KnownNat m, KnownNat n) => Trace (n + 1) -> Trace (m + 1) -> Trace (n + m + 2)
  (TR a) ++ (TR b) = q 
    where
      a' = pack a
      b' = pack b
      res = unpack $ a' ++# b'
      sz  = natVal (Proxy @(n + m + 2))
      q   = case sz of
            4 -> probe (showHW res) $ TR res
            _ -> TR res

  --(++) :: KnownNat m => Trace n -> Trace m -> Trace (n + m)
  --(TR a) ++ (TR b) = q 
  --  where
  --    a' = pack a
  --    b' = pack b
  --    res = unpack $ a' ++# b' :: Unsigned (n + m)
  --    n   = natVal q -- find another method for retrieving n (see CLaSH src!)
  --    q   = case n of
  --          4 -> probe (showHW res) $ TR res
  --          _ -> TR res -- :: Trace (n + m)



-- | Data Constructors |-------------------------------------------------------

tr8 :: Unsigned 8 -> Trace 8
tr8 = literal . toInteger

tr4 :: Unsigned 4 -> Trace 4
tr4 = literal . toInteger

tr2 :: Unsigned 2 -> Trace 2
tr2 = literal . toInteger

tr1 :: Unsigned 1 -> Trace 1
tr1 = literal . toInteger
