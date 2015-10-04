{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module DywaPitchTrack.Internal.Bindings (
    DywaPitchTrackPtr
  , SamplesPtr
  , castToPtrDouble
  , sampleSize
  , withDywaPitchTrack
  , dywapitchNeededSampleCount
  , dywapitchInitTracking
  , dywapitchComputePitch
  ) where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

data DywaPitchTrack

type DywaPitchTrackPtr = Ptr DywaPitchTrack

type SamplesPtr = Ptr CDouble

castToPtrDouble :: CString -> Ptr CDouble
castToPtrDouble = castPtr

sampleSize :: Int
sampleSize = sizeOf (undefined :: CDouble)

withDywaPitchTrack :: (Ptr DywaPitchTrack -> IO a) -> IO a
withDywaPitchTrack = allocaBytes (sizeOf (undefined :: CInt) + sizeOf (undefined :: CDouble))

foreign import ccall safe "dywapitch_neededsamplecount"
    dywapitchNeededSampleCount :: CInt -> IO CInt

foreign import ccall safe "dywapitch_inittracking"
    dywapitchInitTracking :: Ptr DywaPitchTrack -> IO ()

foreign import ccall safe "dywapitch_computepitch"
    dywapitchComputePitch :: Ptr DywaPitchTrack -> Ptr CDouble -> CInt -> CInt -> IO CDouble
