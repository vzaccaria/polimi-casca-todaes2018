module EvalAstReprBench where

import Prelude
import Criterion.Main

import Backend.AST.AstRepr
import Backend.AST.EvalAstRepr
import Primitives.Math         (inv4)


----------------
-- BENCHMARKS --
----------------

main =
  let vars = genVar ("din", 42)
  in  defaultMain [
        bgroup "evalA4 inv4" $
          [ bench "1" $ whnf (evalA4 vars) $ inv4 (var4 "din")
          , bench "4" $ whnf (evalA4 vars) $ inv4 $ inv4 $ inv4 $ inv4 (var4 "din")
          ]
        ]
