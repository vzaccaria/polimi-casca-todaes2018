{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Backend.Trace.Probe
  (probe, showHW)
where

import CLaSH.Prelude    (Unsigned)
import Control.Monad
import Data.List        (partition)
import GHC.TypeLits     (KnownNat, type (+))
import Foreign.C.String (CString, withCString)
import System.IO.Unsafe (unsafePerformIO)


-- | Probing |-----------------------------------------------------------------

-- | Compute the Hamming Weight of intermediates.
--   NOTE: showing the full value for greater correlation (fewer traces).
showHW :: (KnownNat (n + 1)) => Unsigned (n + 1) -> String
showHW = show

--showHW :: Unsigned 4 -> String
--showHW x = show $ popCount x


{-# NOINLINE probe #-}
probe :: String -> a -> a
probe string expr = unsafePerformIO $ do
    probeIO string
    return expr


-- | TraceIO |-----------------------------------------------------------------
-- | (this is the original traceIO from the standard library) -----------------

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
