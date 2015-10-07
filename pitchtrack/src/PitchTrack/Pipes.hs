module PitchTrack.Pipes (
    samplesFromHandle
  , samplesFromLBS
  , getPitch
  , forPitch
  , forPitch_
  , printPitch
  ) where

import           DywaPitchTrack

import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Pipes
import qualified Pipes.ByteString     as PB
import qualified Pipes.Prelude        as P
import           System.IO

-- | Stream chunks of a fixed number of samples from a handle
samplesFromHandle :: Handle -> Producer ByteString PitchTrack ()
samplesFromHandle h = do
    sampleNum <- lift askSampleNum
    PB.hGet (sampleNum * sampleSize) h

-- | Stream chunks of a fixed number of samples from a lazy 'LBS.ByteString'
samplesFromLBS :: LBS.ByteString -> Producer ByteString PitchTrack ()
samplesFromLBS lbs = do
    sampleNum <- lift askSampleNum
    PB.fromLazy lbs >-> PB.take sampleNum

-- | Stream computed pitches
getPitch :: Producer ByteString PitchTrack () -> Producer Double PitchTrack ()
getPitch samplesProducer = samplesProducer >-> P.mapM computePitch

-- | Apply a function to each pitch
forPitch :: Producer ByteString PitchTrack () -> (Double -> PitchTrack a) -> Producer a PitchTrack ()
forPitch samplesProducer f = getPitch samplesProducer >-> P.mapM f

-- | Consume all pitches, applying a function to each one
forPitch_ :: Producer ByteString PitchTrack () -> (Double -> PitchTrack ()) -> Effect PitchTrack ()
forPitch_ samplesProducer f = getPitch samplesProducer >-> P.mapM_ f

-- | Print all pitches
printPitch :: Producer ByteString PitchTrack () -> Effect PitchTrack ()
printPitch samplesProducer = getPitch samplesProducer >-> P.print
