{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE LambdaCase   #-}

module Backend.AST.EvalAstRepr where

import Prelude
import CLaSH.Prelude
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as CH
import qualified Data.Map.Strict       as Map

import Backend.AST.AstRepr
import Language.Operators


type VName = BS.ByteString -- var names
type VVal  = Int           -- values for all types of `Unsigned n` vars.

type Vars  = Map.Map VName VVal -- mapping {name -> val} for all variables


-- | Post-order AST visit.

evalA1 :: Vars -> (AstRepr U1) -> BitVector 1
evalA1 !v = \case
  Get2 a nn  -> (evalA2 v a) ! nn
  Get4 a nn  -> (evalA4 v a) ! nn
  Get8 a nn  -> (evalA8 v a) ! nn
  N a        -> complement (evalA1 v a)
  A a b      -> (evalA1 v a) .&. (evalA1 v b)
  X a b      -> (evalA1 v a) `xor` (evalA1 v b)

evalA2 :: Vars -> (AstRepr U2) -> Unsigned 2
evalA2 !v = \case
  Lit2  n     -> fromIntegral n
  Var2  !s    -> fromIntegral $! v Map.! s
  X2    a b   -> (evalA2 v a) `xor` (evalA2 v b)
  Pack2 a1 a2 -> let
                   x1 = pack . (evalA1 v) $ a1
                   x2 = pack . (evalA1 v) $ a2
                 in  unpack $ x1 ++# x2

evalA4 :: Vars -> (AstRepr U4) -> Unsigned 4
evalA4 !v = \case
  Lit4  n   -> fromIntegral n
  Var4  !s  -> fromIntegral $! v Map.! s
  X4    a b -> (evalA4 v a) `xor` (evalA4 v b)
  Pack4 a b c d -> let
                     x1 = pack . (evalA1 v) $ a
                     x2 = pack . (evalA1 v) $ b
                     x3 = pack . (evalA1 v) $ c
                     x4 = pack . (evalA1 v) $ d
                   in  unpack $ x1 ++# x2 ++# x3 ++# x4

evalA8 :: Vars -> (AstRepr U8) -> Unsigned 8
evalA8 !v = \case
  Var   !s    -> fromIntegral $! v Map.! s
  Pack8 a1 a2 -> let
                   x1 = evalA4 v a1
                   x2 = evalA4 v a2
                 in  (resize x1 `shiftL` 4) .|. resize x2



-----------
-- USAGE --
-----------

genVar :: (String, Int) -> Vars
genVar !(s, a) = Map.singleton (CH.pack s) a

-- λ import Primitives.Math
-- λ let vars = genVar ("din", 42)
-- λ let ast = inv4 (var4 "din")
-- λ evalA4 vars ast
-- λ evalA4 vars $ inv4 $ inv4 $ inv4 $ inv4 (var4 "din")
-- λ evalA4 vars $ inv4 $ inv4 $ inv4 $ inv4 $ inv4 (var4 "din")
