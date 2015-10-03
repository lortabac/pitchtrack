{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DywaPitchTrack (
    PitchTrack
  , runPitchTrack
  , computePitch
  , neededSampleNum
  , wordSize
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
runPitchTrack :: Int -- ^ number of samples to be used for each computation
    -> PitchTrack a
    -> IO a
runPitchTrack sampleNum f = withDywaPitchTrack $ \ptr -> do
    dywapitchInitTracking ptr
    runReaderT (unPitchTrack f) (ptr, sampleNum)

-- | Compute the pitch
computePitch :: ByteString -- ^ samples
    -> PitchTrack Double -- ^ computed pitch
computePitch rawSample = PitchTrack $ do
    (ptr, sampleNum) <- ask
    liftIO $ B.useAsCString rawSample $ \cString ->
        liftIO $ realToFrac <$>
        dywapitchComputePitch ptr (castToPtrDouble cString) 0 (fromIntegral sampleNum)

-- | Calculate the number of samples needed, based on the lowest frequency
neededSampleNum :: Int -> Int
neededSampleNum n = unsafePerformIO $
    fromIntegral <$> dywapitchNeededSampleCount (fromIntegral n)
