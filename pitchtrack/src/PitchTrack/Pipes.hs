module PitchTrack.Pipes (
    samplesFromHandle
  , samplesFromLBS
  , forPitch
  , forPitch_
  , getPitch
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
samplesFromHandle :: Int -> Handle -> Producer ByteString PitchTrack ()
samplesFromHandle sampleNum = PB.hGet (sampleNum * sampleSize)

-- | Stream chunks of a fixed number of samples from a lazy 'LBS.ByteString'
samplesFromLBS :: Int -> LBS.ByteString -> Producer ByteString PitchTrack ()
samplesFromLBS sampleNum lbs = PB.fromLazy lbs >-> PB.take sampleNum

-- | Apply a function to each pitch
forPitch :: Producer ByteString PitchTrack () -> (Double -> PitchTrack a) -> Producer a PitchTrack ()
forPitch samplesProducer f = getPitch samplesProducer >-> P.mapM f

-- | Consume all pitches, applying a function to each one
forPitch_ :: Producer ByteString PitchTrack () -> (Double -> PitchTrack ()) -> Effect PitchTrack ()
forPitch_ samplesProducer f = getPitch samplesProducer >-> P.mapM_ f

-- | Stream computed pitches
getPitch :: Producer ByteString PitchTrack () -> Producer Double PitchTrack ()
getPitch samplesProducer = samplesProducer >-> P.mapM computePitch

-- | Print all pitches
printPitch :: Producer ByteString PitchTrack () -> Effect PitchTrack ()
printPitch samplesProducer = getPitch samplesProducer >-> P.print
