{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}


module Backend.CLaSH.Operators 
  ( -- * Data Types
    HW (..)
    -- * Construction
  , hw8, hw4, hw2, hw1
  )
where


import CLaSH.Prelude        (BitVector, Signal, Unsigned, (++#), fromIntegral, 
                            fromInteger, pack, resize, signal, unpack)
import CLaSH.Sized.Unsigned
import Data.Bits
import GHC.TypeLits
import Prelude              (($), (.), Integer, Eq, Show)

import qualified CLaSH.Prelude as C ((!))
import Language.Operators


-- | Concrete Data Type |------------------------------------------------------

data HW (n :: Nat) = HW { unHW :: Unsigned n } deriving (Eq, Show)

--data DatHW (n :: Nat) = Unsigned n | BitVector 1 deriving (Eq, Show) 
--data DatHW :: Nat -> * where
--  One  :: BitVector 1
--  Many :: Unsigned n


-- | Symantics Instance |------------------------------------------------------

instance Symantics HW where
  literal         = HW . fromInteger
  (HW a) * (HW b) = HW $ a .&. b
  (HW a) + (HW b) = HW $ a `xor` b
  neg (HW x)      = HW $ complement x
  (HW x) ! i      = HW . unpack $ x C.! i
  (HW v1) ++ (HW v2) = HW $ unpack $ v1' ++# v2'
    where
      v1' = pack v1
      v2' = pack v2
  --(HW vs) ++ hwv@(HW v) = HW $ vs' .|. v
  --  where
  --    vs' = shiftL v (fromInteger (natVal vs))


-- | Data Constructors |-------------------------------------------------------

hw8 :: Integer -> HW 8
hw8 = literal

hw4 :: Integer -> HW 4
hw4 = literal

hw2 :: Integer -> HW 2
hw2 = literal

hw1 :: Integer -> HW 1
hw1 = literal
