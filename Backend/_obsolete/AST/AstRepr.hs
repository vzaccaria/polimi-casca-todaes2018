{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Backend.AST.AstRepr where


import Prelude
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as CH

import Language.Operators



------------------
--- Data Types ---
------------------


data family AstRepr a
data instance AstRepr U8 = Var BS.ByteString
                         | Pack8 (AstRepr U4) (AstRepr U4)
                         deriving (Eq, Ord)
data instance AstRepr U4 = Lit4 {-# UNPACK #-} !Int
                         | Var4 BS.ByteString
                         | Pack4 (AstRepr U1) (AstRepr U1) (AstRepr U1) (AstRepr U1)
                         | X4    (AstRepr U4) (AstRepr U4)
                         deriving (Eq, Ord)
data instance AstRepr U2 = Lit2  {-# UNPACK #-} !Int
                         | Var2  BS.ByteString
                         | Pack2 (AstRepr U1) (AstRepr U1)
                         | X2    (AstRepr U2) (AstRepr U2)
                         deriving (Eq, Ord)
data instance AstRepr U1 = Get2 (AstRepr U2) {-# UNPACK #-} !Int
                         | Get4 (AstRepr U4) {-# UNPACK #-} !Int
                         | Get8 (AstRepr U8) {-# UNPACK #-} !Int
                         | N (AstRepr U1)
                         | A (AstRepr U1) (AstRepr U1)
                         | X (AstRepr U1) (AstRepr U1)
                         deriving (Eq, Ord)



-----------------------------------------------
--- AST Interpretation of Polymorphic Specs ---
-----------------------------------------------


instance Symantics AstRepr where
  literal2 !x = Lit2 $! fromInteger x
  literal4 !x = Lit4 $! fromInteger x

  n = N

  a .&  b  =  A  a b
  a .^  b  =  X  a b
  a .^. b  =  X2 a b
  a ..^ b  =  X4 a b

  a .! nn    = Get2 a nn
  a ..! nn   = Get4 a nn
  a ...! nn  = Get8 a nn

  pack2 a b     = Pack2 a b
  pack8 a b     = Pack8 a b
  pack4 a b c d = Pack4 a b c d



-------------------------
--- Data Constructors ---
-------------------------

var2 :: String -> (AstRepr U2)
var2 !s = Var2 $! CH.pack s

var4 :: String -> (AstRepr U4)
var4 !s = Var4 $! CH.pack s

var8 :: String -> (AstRepr U8)
var8 !s = Var  $! CH.pack s
