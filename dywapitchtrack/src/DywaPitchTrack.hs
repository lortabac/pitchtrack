{- |
Note that because all parameters are hard-coded into the C library,
you are limited to the following audio configuration:

  * raw (headerless) data

  * a sampling rate of 44100Hz,

  * a sample size of @sizeof(double)@

  * floating-point encoding

  * one channel (mono)
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DywaPitchTrack (
    PitchTrack
  , runPitchTrack
  , computePitch
  , neededSampleNum
  , sampleSize
  ) where

import           DywaPitchTrack.Internal.Bindings

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as B
import           System.IO.Unsafe

newtype PitchTrack a = PitchTrack
    { unPitchTrack :: ReaderT (DywaPitchTrackPtr, Int) IO a
    } deriving (Functor, Applicative, Monad, MonadIO)

-- | Run the 'PitchTrack' monad
runPitchTrack :: Int -- ^ Number of samples to be used for each computation
    -> PitchTrack a -- ^ Computations
    -> IO a
runPitchTrack sampleNum f = withDywaPitchTrack $ \ptr -> do
    dywapitchInitTracking ptr
    runReaderT (unPitchTrack f) (ptr, sampleNum)

-- | Compute the pitch.
--
-- The size of the ByteString must be equal to
-- the number of samples set in 'runPitchTrack' * the size of each sample ('sampleSize').
--
-- Note: this pre-condition is __not__ checked!
computePitch :: ByteString -- ^ Samples
    -> PitchTrack Double -- ^ Computed pitch
computePitch rawSample = PitchTrack $ do
    (ptr, sampleNum) <- ask
    liftIO $ B.useAsCString rawSample $ \cString ->
        liftIO $ realToFrac <$>
        dywapitchComputePitch ptr (castToPtrDouble cString) 0 (fromIntegral sampleNum)

-- | Calculate the number of samples needed, based on the lowest frequency
neededSampleNum :: Int -- ^ Lowest frequency, in Hz
    -> Int -- ^ Number of samples needed for each computation
neededSampleNum n = unsafePerformIO $
    fromIntegral <$> dywapitchNeededSampleCount (fromIntegral n)
