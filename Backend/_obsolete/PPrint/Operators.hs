{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Backend.PPrint.Operators 
  (PPrint (..))
where

import Data.Proxy
import GHC.TypeLits (Nat, KnownNat, natVal)

import Language.Operators           hiding ((++))
import qualified Language.Operators as LO ((++))


------------------------
-- Concrete Data Type --
------------------------

data PPrint (n :: Nat) = P { unP :: String }


--------------------------------------------------
-- Symantics class instance for pretty printing --
--------------------------------------------------

instance Symantics PPrint where
  literal x = P $ show x
  (P x) * (P y) = q
    where
      q = P $ "AND" ++ show n ++ "(" ++ x ++ "," ++ y ++")"
      n = natVal q

  (P x) + (P y) = q
    where
      q = P $ "XOR" ++ show n ++ "(" ++ x ++ "," ++ y ++")"
      n = natVal q

  (P x) ! i = P $ x ++ "[" ++ show i ++ "]"
  neg (P x) = P $ "NOT(" ++ x ++ ")"

  (P x) ++ (P y) = undefined

  --pack4 (P x1) (P x2) (P x3) (P x4) = P ("{ " ++ x1 ++ "," ++ x2 ++ "," ++ x3 ++ "," ++ x4 ++ " }")
  --pack8 (P x1) (P x2)               = P $ "{ " ++ x1 ++ "," ++ x2 ++ " }"
