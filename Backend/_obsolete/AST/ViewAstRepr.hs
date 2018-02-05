{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}

module ViewAstRepr where

import Prelude
import CLaSH.Prelude hiding ((++))

import Backend.AST.AstRepr
import Language.Operators


viewA1 :: (AstRepr U1) -> String
viewA1 = \case
  Get2 a nn  -> (viewA2 a) ++ (".!" ++ show nn)
  Get4 a nn  -> (viewA4 a) ++ ("..!" ++ show nn)
  Get8 a nn  -> (viewA8 a) ++ ("...!" ++ show nn)
  N a        -> "N" ++ (viewA1 a)
  A a b      -> (viewA1 a) ++ "A" ++ (viewA1 b)
  X a b      -> (viewA1 a) ++ "X" ++ (viewA1 b)

viewA2 :: (AstRepr U2) -> String
viewA2 = \case
  Lit2  n     -> show n
  Var2  s     -> s
  X2    a b   -> (viewA2 a) ++ "X2" ++ (viewA2 b)
  Pack2 a1 a2 -> (viewA1 a2) ++ (viewA1 a2) ++ "P2"

viewA4 :: (AstRepr U4) -> String
viewA4 = \case
  Lit4  n   -> show n
  Var4  s   -> s
  X4    a b -> (viewA4 a) ++ "X4" ++ (viewA4 b)
  Pack4 a b c d -> (viewA1 a) ++
                   (viewA1 b) ++
                   (viewA1 c) ++
                   (viewA1 d) ++ "P4"

viewA8 :: (AstRepr U8) -> String
viewA8 = \case
  Var   s     -> s
  Pack8 a1 a2 -> (viewA4 a2) ++ (viewA4 a2) ++ "P8"


-----------
-- USAGE --
-----------

-- λ import Primitives.Math
-- λ let ast = inv4 (var4 "din")
-- λ viewA4 ast
