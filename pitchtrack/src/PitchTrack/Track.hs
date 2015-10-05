{- |
This module provides a high-level interface for the most common cases.

The default number of samples used for each computation is 2048,
which should be enough if you never go below 65Hz.
If you need a different number, you should use the functions whose name ends with @N@,
which take the number of samples as a first parameter.
-}
module PitchTrack.Track (
-- * Reading from a file
    trackFile
  , trackFileN
  , trackFileToList
-- * Reading from stdin
  , trackStdin
  , trackStdinN
-- * Reading from a handle
  , trackHandle
  , trackHandleN
-- * Reading from a lazy 'LBS.ByteString'
  , trackLBS
  , trackLBSN
-- * Other definitions
  , defaultSampleNum
  ) where

import           DywaPitchTrack
import           PitchTrack.Pipes

import qualified Data.ByteString.Lazy as LBS
import           Pipes
import qualified Pipes.Prelude        as P
import           System.IO

-- | Track a file and apply a function to each computed pitch
--
-- >>> trackFile "a440.raw" $ \pitch -> liftIO $ putStr (show pitch ++ ",")
-- 440.0,440.0,440.0,440.0,440.0,440.0,440.0,440.0,440.0,440.0,440.0,440.0,
trackFile :: FilePath -> (Double -> PitchTrack ()) -> IO ()
trackFile = trackFileN defaultSampleNum

-- | Same as 'trackFile', but takes the number of samples as a first parameter
trackFileN :: Int -> FilePath -> (Double -> PitchTrack ()) -> IO ()
trackFileN sampleNum file f = withFileR file $ \ h->
    trackHandleN sampleNum h f

-- | Same as 'trackFile' but reads from 'System.IO.stdin' instead of a file
trackStdin :: (Double -> PitchTrack ()) -> IO ()
trackStdin = trackStdinN defaultSampleNum

-- | Same as 'trackStdin', but takes the number of samples as a first parameter
trackStdinN :: Int -> (Double -> PitchTrack ()) -> IO ()
trackStdinN sampleNum = trackHandleN sampleNum stdin

-- | Same as 'trackFile' but reads from a handle instead of a file
trackHandle :: Handle -> (Double -> PitchTrack ()) -> IO ()
trackHandle = trackHandleN defaultSampleNum

-- | Same as 'trackHandle', but takes the number of samples as a first parameter
trackHandleN :: Int -> Handle -> (Double -> PitchTrack ()) -> IO ()
trackHandleN sampleNum h f = runPitchTrack sampleNum $
    runEffect $ forPitch_ (samplesFromHandle sampleNum h) f

-- | Track a lazy 'LBS.ByteString' and apply a function to each computed pitch
trackLBS :: LBS.ByteString -> (Double -> PitchTrack ()) -> IO ()
trackLBS = trackLBSN defaultSampleNum

-- | Same as 'trackLBS', but takes the number of samples as a first parameter
trackLBSN :: Int -> LBS.ByteString -> (Double -> PitchTrack ()) -> IO ()
trackLBSN sampleNum lbs f = runPitchTrack sampleNum $
    runEffect $ forPitch_ (samplesFromLBS sampleNum lbs) f

-- | Track a file and return a list of all the computed pitches
--
-- Note: the whole list is loaded into memory
trackFileToList :: FilePath -> IO [Double]
trackFileToList = trackFileToListN defaultSampleNum

trackFileToListN  :: Int -> FilePath -> IO [Double]
trackFileToListN sampleNum file = withFileR file $ \h ->
    runPitchTrack sampleNum $ P.toListM $ forPitch (samplesFromHandle sampleNum h) return

-- | The default number of samples used for each computation (2048)
defaultSampleNum :: Int
defaultSampleNum = 2048

withFileR :: FilePath -> (Handle -> IO r) -> IO r
withFileR file = withFile file ReadMode
