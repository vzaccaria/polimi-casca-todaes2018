{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE LambdaCase   #-}

module Backend.AST.EvalCount where

import Prelude
import CLaSH.Prelude

import Backend.AST.AstRepr
import Language.Operators


-- | Post-order AST visit.

countA1 :: (AstRepr U1) -> Integer
countA1 = \case
  Get2 a _ -> (countA2 a) + 1
  Get4 a _ -> (countA4 a) + 1
  Get8 a _ -> (countA8 a) + 1
  N a      -> (countA1 a) + 1
  A a b    -> (countA1 a) + (countA1 b) + 1
  X a b    -> (countA1 a) + (countA1 b) + 1

countA2 :: (AstRepr U2) -> Integer
countA2 = \case
  Lit2  _     -> 1
  Var2  _     -> 1
  X2    a b   -> (countA2 a)  + (countA2 b)  + 1
  Pack2 a1 a2 -> (countA1 a1) + (countA1 a2) + 1

countA4 :: (AstRepr U4) -> Integer
countA4 = \case
  Lit4  _     -> 1
  Var4  _     -> 1
  X4    a b   -> (countA4 a) + (countA4 b) + 1
  Pack4 a b c d -> (countA1 a) +
                   (countA1 b) +
                   (countA1 c) +
                   (countA1 d) + 1
                   
countA8 :: (AstRepr U8) -> Integer
countA8 = \case
  Var   _     -> 1
  Pack8 a1 a2 -> (countA4 a1) + (countA4 a2) + 1


-----------
-- USAGE --
-----------

-- λ import Primitives.Math
-- λ let ast = inv4 (var4 "din")
-- λ countA4 ast

-- λ countA4 $ inv4 (var4 "din")
-- 144
-- λ countA4 $ inv4 $ inv4 (var4 "din")
-- 7151
-- λ countA4 $ inv4 $ inv4 $ inv4 (var4 "din")
-- 350494
-- λ countA4 $ inv4 $ inv4 $ inv4 $ inv4 (var4 "din")
-- 17174301
-- λ countA4 $ inv4 $ inv4 $ inv4 $ inv4 $ inv4 (var4 "din")
-- 841540844
