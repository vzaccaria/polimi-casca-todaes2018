{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

--  Using SWord4, SWord2

module Backend.SBV.Operators2 where

import qualified Data.SBV as S
--import Data.SBV.Examples.Misc.Word4
--import qualified Data.SBV.Examples.Crypto.AES as A

import Data.Word
import Data.Bits
import Language.Operators
import Backend.SBV.SWord4 (SWord4)

data family Symbolic a
data instance Symbolic U8 = SY8 { unSY8 :: S.SWord8 }
data instance Symbolic U4 = SY4 { unSY4 :: SWord4 }
data instance Symbolic U2 = SY2 { unSY2 :: S.SWord8 }
data instance Symbolic U1 = SY1 { unSY1 :: S.SWord8 }

-- FIX? it's currently impossible to fully implement SWord4, SWord2, ...
--      without any access to SBV internal functions.


sy1 :: S.SWord8 -> Symbolic U1
sy1 = SY1 . (S..&. 0x1)

sy2 :: S.SWord8 -> Symbolic U2
sy2 = SY2 . (S..&. 0x3)

sy4 :: SWord4 -> Symbolic U4
sy4 = SY4
--sy4 :: S.SWord8 -> Symbolic U4
--sy4 = SY4 . (S..&. 0xf)



instance Symantics Symbolic where
  literal2 = sy2 . fromInteger
  literal4 = sy4 . fromInteger

  n x      = sy1 $ S.complement (unSY1 x)

  (SY1 a) .&  (SY1 b)  =  sy1 $  a  S..&.  b
  (SY1 a) .^  (SY1 b)  =  sy1 $  a `S.xor` b
  (SY2 a) .^. (SY2 b)  =  sy2 $  a `S.xor` b
  (SY4 a) ..^ (SY4 b)  =  sy4 $  a `S.xor` b

  -- FIX? use Splittable
  pack2 (SY1 x1) (SY1 x2)                   = sy2 $ (x1 `shiftL` 1) S..|. x2
  pack4 (SY1 x1) (SY1 x2) (SY1 x3) (SY1 x4) = sy4 . snd . S.split $ x1' S..|. x2' S..|. x3' S..|. x4'
    where
      x1' = x1 `shiftL` 3
      x2' = x2 `shiftL` 2
      x3' = x3 `shiftL` 1
      x4' = x4
  pack8 (SY4 a) (SY4 b)                     = SY8 $ a S.# b
  
  -- FIX: refactor these
  (SY2 a) .! i     = SY1 val
    where
      val = S.ite b (S.literal 0x1) (S.literal 0x0)
      b   = S.sbvTestBit a i
  (SY4 a) ..! i     = SY1 val
    where
      val = S.ite b (S.literal 0x1) (S.literal 0x0)
      b   = S.sbvTestBit a i
  (SY8 a) ...! i    = SY1 val
    where
      val = S.ite b (S.literal 0x1) (S.literal 0x0)
      b   = S.sbvTestBit a i
  




