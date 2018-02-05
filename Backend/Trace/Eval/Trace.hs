{-# LANGUAGE DataKinds #-}


module Backend.Trace.Eval.Trace where


import CLaSH.Prelude    ((++#), BitVector, Unsigned)
import Control.DeepSeq
import Control.Monad
import Data.Bits
import Prelude          hiding ((++))
import System.IO
import Test.Tasty
import Test.Tasty.HUnit

import qualified Prelude               as P ((++))
import qualified Debug.Trace           as D
import qualified Test.Tasty.QuickCheck as QC

import Language.Operators
import Backend.Trace.Operators
import Backend.Trace.Eval.Properties
import Primitives.MaskedSBOX         (msbox)


-- | Tracing procedures |------------------------------------------------------

-- | Generate N random inputs for the high-level simulation.
-- The inputs here are (8-bit, 4-bit, 8-bit) triplets.
-- Usage example:
-- λ stack exec ghc -- Backend/Trace/Eval/Trace.hs -e "generateNInputs 1000" > inputs.dat
generateNInputs n = replicateM_ n $ (QC.generate QC.arbitrary :: IO (Unsigned 8, Unsigned 4, Unsigned 8)) >>= print


-- | Apply N random inputs to the selected primitive, in order to
--   Generate high-level simulation traces of the selected primitive, starting from an input vector file.
-- Usage example:
-- λ stack exec ghc -- Backend/Trace/Eval/Trace.hs -e 'applyNInputs testNOMASKS    "inputs.dat"' 2> trace_NOMASKS.dat > /dev/null
-- λ stack exec ghc -- Backend/Trace/Eval/Trace.hs -e 'applyNInputs testWithFreshM "inputs.dat"' 2> trace_WithFreshM.dat > /dev/null
-- λ stack exec ghc -- Backend/Trace/Eval/Trace.hs -e 'applyNInputs testNoFreshM   "inputs.dat"' 2> trace_NoFreshM.dat > /dev/null
-- λ stack exec ghc -- Backend/Trace/Eval/Trace.hs -e 'applyNInputs testBrokenOsw  "inputs.dat"' 2> trace_BrokenOsw.dat > /dev/null
-- λ stack exec ghc -- Backend/Trace/Eval/Trace.hs -e 'applyNInputs testDepMasks   "inputs.dat"' 2> trace_DepMasks.dat > /dev/null
-- λ stack exec ghc -- Backend/Trace/Eval/Trace.hs -e 'applyNInputs testDepMaskHI  "inputs.dat"' 2> trace_DepMaskHI.dat > /dev/null
-- λ stack exec ghc -- Backend/Trace/Eval/Trace.hs -e 'applyNInputs testDepMaskLO  "inputs.dat"' 2> trace_DepMaskLO.dat > /dev/null
applyNInputs f fpath = do
    handle <- openFile fpath ReadMode
    ts <- map read <$> lines <$> hGetContents handle :: IO [(Integer, Integer, Integer)]
    --mapM_ (putStrLn . show . f . makeTestInputs) ts
    mapM_ (singleTrace . f . makeTestInputs) ts
    return ()


-- | Print the inputs of the SBOX, from the input vector and the (hardcoded) secret key.
-- Note: the (target) secret key is hardcoded in `makeTestInputs`, where it's added to the plaintext.
-- Usage example:
-- λ stack exec ghc -- Backend/Trace/Eval/Trace.hs -e 'printSBoxIN "inputs.dat"' 2> /dev/null > sboxIN.dat
printSBoxIN fpath = do
    handle <- openFile fpath ReadMode
    ts <- map read <$> lines <$> hGetContents handle :: IO [(Integer, Integer, Integer)]
    mapM_ (putStrLn . show . unTR . (\(_,_,x) -> x) . makeTestInputs) ts
    return ()


-- | Generate 256 Hypothesis (zeroed masks)
-- $ stack exec ghc -- Backend/Trace/Debug2.hs -e "gen256hyps \"Backend/Trace/Data/inputs.dat\"" 2> Backend/Trace/Data/Hyps256.dat
gen256hyps fpath = do
  mapM_ (flip genSingleHyp $ fpath) [0..255]


-------------------------------------------------------------------------------


singleTrace :: (Show s) => s -> IO ()
singleTrace s = do
  (show s) `deepseq` D.traceIO "" -- put a newline on stderr
  --putStrLn $ show s
  --D.traceIO "" 
  --(show s) `seq` D.traceIO ""


genSingleHyp key fpath = do
    putStrLn $ "Key: " P.++ show key
    handle <- openFile fpath ReadMode
    ts <- map read <$> lines <$> hGetContents handle :: IO [(Integer, Integer, Integer)]
    mapM_ (singleTrace . testHYPS . (runTest key)) ts 
    return ()


-------------------------------------------------------------------------------


runTest :: Integer -> (Integer, Integer, Integer) -> (Trace 8, Trace 4, Trace 8)
runTest ikey (m, mf, x) = (m', mf', x')
  where
    --key = tr8 0x5A -- fixed secret key byte

    m'  = tr8 $ fromInteger m
    mf' = tr4 $ fromInteger mf
    x'  = tr8 $ (fromInteger x) `xor` (unTR . tr8 . fromInteger $ ikey) -- plaintext XOR key


-------------------------------------------------------------------------------


makeTestInputs :: (Integer, Integer, Integer) -> (Trace 8, Trace 4, Trace 8)
makeTestInputs (m, mf, x) = (m', mf', x')
  where
    key = tr8 0x5A -- fixed secret key byte

    m'  = tr8 $ fromInteger m
    mf' = tr4 $ fromInteger mf
    x'  = tr8 $ (fromInteger x) `xor` (unTR key :: Unsigned 8) -- plaintext XOR {key=0x5A}


--testNOMASKS    (_, _ , x) = msbox (tr8 0, tr4 0) x
testHYPS :: (Trace 8, Trace 4, Trace 8) -> Trace 8
testHYPS (TR mu, _ , TR xu) =
  let y = tr8 $ xu `xor` mu -- unmasked input!
  in  msbox (tr8 0, tr4 0) y

