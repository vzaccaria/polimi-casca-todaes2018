-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
-- {-# LANGUAGE InstanceSigs        #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}


module Backend.SBV.Operators 
  ( -- * Data Types
    SY (..)
    -- * Construction
  , sy, sy1, sy2, sy4
  )
where

import Data.Bits
import Data.Proxy
import Data.SBV
--import Data.SBV   ((.&.), (.|.), SWord8)
import GHC.TypeLits (Nat, KnownNat, type (+), natVal)
import Prelude

import qualified Data.SBV as S
import qualified Prelude  as P ((+))

import Language.Operators

--import Data.Word
--import Backend.SBV.Word4 as W
--import Data.SBV.Examples.Misc.Word4


sbvTestBit = S.sTestBit


-- | Concrete Data Type for formal verification |----------------------------------------

data SY (n :: Nat) = SY { unSY :: SWord8 } deriving (Show)
--   FIX? it's currently impossible to fully implement SWord4, SWord2, ...
--   types without any access to SBV internal functions.


-- | Symantics instance for the SBV backend. |-------------------------------------------

instance Symantics SY where
  literal x = q
    where q = sy . fromInteger $ x

  (SY a) * (SY b) = sy $ a .&. b
  (SY a) + (SY b) = sy $ a `xor` b
  neg (SY x)      = sy $ S.complement x

  (SY x) ! i = SY val
    where
      val = S.ite b (S.literal 0x1) (S.literal 0x0)
      b   = sbvTestBit x i

  syv1@(SY v1) ++ syv2@(SY v2) = q
    where
      q  = SY . (syConstraints' $ sz) $ (v1 `shiftL` n2) .|. v2
      n2 = fromInteger $ natVal syv2 :: Int
      n1 = fromInteger $ natVal syv1 :: Int
      sz = toInteger $ n1 P.+ n2
      

-- | "Smart" value constructors |----------------------------------------------

sy1 :: SWord8 -> SY 1
sy1 = sy

sy2 :: SWord8 -> SY 2
sy2 = sy

sy4 :: SWord8 -> SY 4
sy4 = sy

sy :: forall n. KnownNat n => SWord8 -> SY n
sy = SY . (syConstraints sz) 
  where
    sz = natVal (Proxy @n)


-- Keep private. --------------------------------------------------------------


-- | Constrain SWord8 to the appropriate domain.
syConstraints :: (Bits b, Num b) => Integer -> (b -> b)
syConstraints n = case n of
    1 -> (.&. 0x1)
    2 -> (.&. 0x3)
    4 -> (.&. 0xf)
    8 -> id
    _ -> undefined

syConstraints' :: (Bits b, Num b) => Integer -> (b -> b)
syConstraints' n
    | any (==n) [1, 2, 4] = syConstraints n
    | otherwise = id
