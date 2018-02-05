{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}


module Language.Operators 
  ( -- * Data Types
    Symantics (..)
  )
where

import GHC.TypeLits (KnownNat, Nat, type (+), natVal)
import Prelude      hiding ((++))


-- | Typed Final Tagless polymorphic language |-------------------------------------------

infixl 8  !
infixl 7  *
infixl 6  +

class Symantics (repr :: Nat -> *) where
  literal :: KnownNat (n + 1) => Integer -> repr (n + 1)
  (*)     :: KnownNat (n + 1) => repr (n + 1) -> repr (n + 1) -> repr (n + 1)
  (+)     :: KnownNat (n + 1) => repr (n + 1) -> repr (n + 1) -> repr (n + 1)
  (!)     :: KnownNat n => repr n -> Int -> repr 1
  neg     :: repr 1 -> repr 1
  (++)    :: (KnownNat n, KnownNat m) => repr (n + 1) -> repr (m + 1) -> repr (n + m + 2)
