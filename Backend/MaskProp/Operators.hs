{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}


{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver   #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise         #-}
-- {-# OPTIONS_GHC -dcore-lint #-}


module Backend.MaskProp.Operators
  ( -- * Datatypes
    Tag (..)
  , NatMP (..)
    -- * Construction
  , mp, (~>)
    -- * Type classes
  , GFValidMask (..)
  )
where

import Prelude             hiding ((++), (!!), (+), (*))
import CLaSH.Sized.Vector  hiding ((++), (!!), repeat, zipWith)
import Control.Applicative
import Data.Monoid
import Data.Set            ((\\), Set)
import GHC.TypeLits        (KnownNat, Nat, type (+))

import qualified Prelude            as P
import qualified CLaSH.Sized.Vector as V
import qualified Data.Set           as S

import Language.Operators


-- | Concrete Data Type |------------------------------------------------------

data NatMP (n :: Nat) = MP Tag (Maybe (Vec n GFMaskSet)) deriving (Show)


-- | Symantics Instance |------------------------------------------------------

instance Symantics NatMP where
  --literal :: KnownNat (n + 1) => Integer -> NatMP (n + 1)
  literal _ = mp C $ Just $ V.repeat S.empty
  neg = id
  (*) = (+)
  (MP t1 x) + (MP t2 y) = mp (t1 <> t2) $ vcombine x y
  (MP t vs) ! i         = mp t $ (vs !! i) ~> (pure Nil)
  (MP t1 v1) ++ (MP t2 v2) = mp t $ v1 ++@ v2
    where t = t1 <> t2


-- | Other Data Types |--------------------------------------------------------

type GFMaskSet = Set Int

instance {-# OVERLAPPING #-} Show GFMaskSet where
  show s = show $ S.elems s

data Tag = Msk  -- mask bits
         | Dat  -- (masked) data bits
         | C    -- (masked) constant bits
         deriving (Eq, Show)


-- | Type Classes |------------------------------------------------------------

class GFMasked s where
  mcombine :: s -> s -> s

class GFMaskedVector v where
  vcombine :: v -> v -> v

class GFValidMask a where
  mvalid :: a -> Bool


-- | Class Instances |---------------------------------------------------------

-- | The Monoid laws are all verified:
--     (x <> y) <> z = x <> (y <> z) -- associativity
--     mempty <> x = x               -- left identity
--     x <> mempty = x               -- right identity
instance Monoid Tag where
  mempty = C

  mappend Dat  _   = Dat
  mappend _    Dat = Dat
  mappend Msk  _   = Msk
  mappend _    Msk = Msk
  mappend t1   t2  | t1 == t2 = t1   

instance KnownNat n => GFValidMask (NatMP n) where
  mvalid (MP Dat Nothing) = False
  mvalid (MP _   _)       = True

instance GFMasked GFMaskSet where
  mcombine m1 m2 = (S.union m1 m2) \\ (S.intersection m1 m2)

instance (KnownNat n) => GFMaskedVector (Vec n GFMaskSet) where
  vcombine = V.zipWith mcombine

instance (KnownNat n) => GFMaskedVector (Maybe (Vec n GFMaskSet)) where
  vcombine = liftA2 vcombine


-- | Utility operators |-------------------------------------------------------

infixr 5 ++@
(++@) = liftA2 (V.++)

(!!) :: (Applicative f, Enum i, KnownNat n) => f (Vec n a) -> i -> f a
v !! nn = liftA2 (V.!!) v (pure nn)


-- | Data Constructors |-------------------------------------------------------

-- (~>) :: Applicative f => f b -> f (Vec n b) -> f (Vec (n GHC.TypeLits.+ 1) b)
v1 ~> v2 = liftA2 (:>) v1 v2
infixr 5 ~>

mp :: KnownNat (n + 1) => Tag -> Maybe (Vec (n + 1) GFMaskSet) -> NatMP (n + 1)
mp t@Dat v = MP t $ v >>= traverse checkMasking
mp t     v = MP t $ v

-- | Bad-masking error propagation function.
checkMasking :: Set a -> Maybe (Set a)
checkMasking x = case (S.null x) of
            True -> Nothing
            _ -> Just x
