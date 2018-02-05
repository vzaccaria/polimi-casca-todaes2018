{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE Unsafe           #-}


module Backend.Trace.Debug where

import qualified CLaSH.Prelude         as C  ((!), high, pack, resize, shiftL, unpack)
import           CLaSH.Prelude               ((++#), BitVector, Unsigned)
import           Control.Monad
import           Data.Bits
import           Prelude

import qualified Debug.Trace           as D
import           System.IO
import qualified System.IO.Unsafe      as U
import           Data.List                   (partition)
import           Foreign.C.String            (CString, withCString)
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import Language.Operators

import           Primitives.Math22
import           Primitives.Math                         (aff_trans)
import           Primitives.UnmaskedSBOX                 (sbox')
import           Primitives.MaskedSBOX                   (msbox)
import qualified Primitives.Broken.MaskedGFInv as Broken (m_invGF)


data family Trace a
data instance Trace U8 = T8 { unT8 :: Unsigned  8 } deriving (Show)
data instance Trace U4 = T4 { unT4 :: Unsigned  4 } deriving (Show)
data instance Trace U2 = T2 { unT2 :: Unsigned  2 } deriving (Show)
data instance Trace U1 = T1 { unT1 :: BitVector 1 } deriving (Show)


instance Symantics Trace where
  literal2 = T2 . fromInteger
  literal4 = T4 . fromInteger

  n x      = T1 (complement (unT1 x))

  (T1 a) .&  (T1 b)  = T1 $ a .&. b
  (T1 a) .^  (T1 b)  = T1 $ a `xor` b
  (T2 a) .^. (T2 b)  = T2 $ a `xor` b
  (T4 a) ..^ (T4 b)  = T4 w
    where
      p = a `xor` b
      w = probe (show p) $ p
      --w = probe (show4 p) $ p

  pack2 (T1 x1) (T1 x2) = T2 w
    where
      x1' = C.pack x1
      x2' = C.pack x2
      w   = C.unpack $ x1' ++# x2'

  pack4 (T1 x1) (T1 x2) (T1 x3) (T1 x4) = T4 w
    where
      x1' = C.pack x1
      x2' = C.pack x2
      x3' = C.pack x3
      x4' = C.pack x4
      p   = C.unpack $ x1' ++# x2' ++# x3' ++# x4'
      w   = probe (show p) $ p
      --w   = probe (show4 p) $ p

  pack8 (T4 a1) (T4 a2) = T8 w
    where
      w = (C.resize a1 `shiftL` 4) .|. C.resize a2

  (T2 a) .! nn    = T1 $ C.unpack $ a C.! nn
  (T4 a) ..! nn   = T1 $ C.unpack $ a C.! nn
  (T8 a) ...! nn  = T1 $ C.unpack $ a C.! nn



-----------------------
-- Data constructors --
-----------------------


t8 :: Unsigned 8 -> Trace U8
t8 = T8

t4 :: Unsigned 4 -> Trace U4
t4 = T4

t2 :: Unsigned 2 -> Trace U2
t2 = T2

t1 :: BitVector 1 -> Trace U1
t1 = T1


-------------
-- Probing --
-------------

{-# NOINLINE probe #-}
probe :: String -> a -> a
probe string expr = U.unsafePerformIO $ do
    probeIO string
    return expr


-- TODO: use `testBit` for computing the HW.
show4 :: Unsigned 4 -> String
show4 x = (show x1) ++ (show x2) ++ (show x3) ++ (show x4)
  where
    x1 = x C.! 0
    x2 = x C.! 1
    x3 = x C.! 2
    x4 = x C.! 3



-------------
-- TraceIO --
-------------

-- (this is the original traceIO from the standard library)
probeIO :: String -> IO ()
probeIO msg = do
    withCString "%s " $ \cfmt -> do
     -- NB: debugBelch can't deal with null bytes, so filter them
     -- out so we don't accidentally truncate the message.  See Trac #9395
     let (nulls, msg') = partition (=='\0') msg
     withCString msg' $ \cmsg ->
      debugBelch cfmt cmsg
     when (not (null nulls)) $
       withCString "WARNING: previous trace message had null bytes" $ \cmsg ->
         debugBelch cfmt cmsg

foreign import ccall unsafe "HsBase.h debugBelch2"
   debugBelch :: CString -> CString -> IO ()


---------------
-- Utilities --
---------------

-- stack exec ghc -- Backend/Trace/Debug.hs -e "repeatNTimes 10 $ \x -> print $ sbox' $ t8 x" 2> traccia.txt
repeatNTimes 0 _ = return ()
repeatNTimes n action =
 do
  D.traceIO ""
  action n
  repeatNTimes (n-1) $ action


-- λ stack exec ghc -- Backend/Trace/Debug.hs -e "generateNInputs 1000" > inputs.dat
generateNInputs n = replicateM_ n $ (QC.generate QC.arbitrary :: IO (Unsigned 8, Unsigned 4, Unsigned 8)) >>= print

-- fpath == "inputs.dat"
{-
readNInputs fpath =
  do
    handle <- openFile fpath ReadMode
    ts <- map read <$> lines <$> hGetContents handle :: IO [(Integer, Integer, Integer)]
    --putStrLn $ show ts
    mapM_ (putStrLn . show) ts
    return ()
-}

-- λ stack exec ghc -- Backend/Trace/Debug.hs -e 'applyNInputs testNOMASKS    "inputs.dat"' 2> trace_NOMASKS.dat > /dev/null
-- λ stack exec ghc -- Backend/Trace/Debug.hs -e 'applyNInputs testWithFreshM "inputs.dat"' 2> trace_WithFreshM.dat > /dev/null
-- λ stack exec ghc -- Backend/Trace/Debug.hs -e 'applyNInputs testNoFreshM   "inputs.dat"' 2> trace_NoFreshM.dat > /dev/null
-- λ stack exec ghc -- Backend/Trace/Debug.hs -e 'applyNInputs testBrokenOsw  "inputs.dat"' 2> trace_BrokenOsw.dat > /dev/null
-- λ stack exec ghc -- Backend/Trace/Debug.hs -e 'applyNInputs testDepMasks   "inputs.dat"' 2> trace_DepMasks.dat > /dev/null
applyNInputs f fpath =
  do
    handle <- openFile fpath ReadMode
    ts <- map read <$> lines <$> hGetContents handle :: IO [(Integer, Integer, Integer)]
    --mapM_ (putStrLn . show . f . makeTestInputs) ts
    mapM_ (singleTrace . f . makeTestInputs) ts
    return ()

singleTrace :: (Show s) => s -> IO ()
singleTrace s = do
  putStrLn $ show s
  D.traceIO "" -- put a newline on stderr

-----------
-- Tests --
-----------

makeTestInputs :: (Integer, Integer, Integer) -> (Trace U8, Trace U4, Trace U8)
makeTestInputs (a, b, c) = (a', b', c')
  where
    a' = t8 $ fromInteger a
    b' = t4 $ fromInteger b
    c' = t8 $ fromInteger c

testNOMASKS :: (Trace U8, Trace U4, Trace U8) -> Trace U8
--testNOMASKS    (_, _ , x) = msbox (t8 0, t4 0) x
testNOMASKS (T8 mu, _ , T8 xu) =
  let y = t8 $ xu `xor` mu -- unmasked input
  in  msbox (t8 0, t4 0) y

testWithFreshM :: (Trace U8, Trace U4, Trace U8) -> Trace U8
testWithFreshM (m, mf, x) = msbox (m, mf) x

testNoFreshM   :: (Trace U8, Trace U4, Trace U8) -> Trace U8
testNoFreshM   (m, _ , x) = msbox (m, t4 0) x

testBrokenOsw  :: (Trace U8, Trace U4, Trace U8) -> Trace U8
testBrokenOsw  (m, mf, x) = aff_trans . Broken.m_invGF (m, mf) $ x

testDepMasks   :: (Trace U8, Trace U4, Trace U8) -> Trace U8
testDepMasks   (m, mf, x) = undefined 


