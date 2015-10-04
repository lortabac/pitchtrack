module PitchTrack.TrackSpec (spec) where

import           PitchTrack.Track

import           Control.Monad.IO.Class
import           Test.Hspec

spec :: Spec
spec = describe "trackFile" $ do
    it "should find the right frequency (±1Hz)" $ do
        pitches <- liftIO $ trackFileToList a4File
        all (\p -> abs (440.0 - p) < 1) (init pitches) `shouldBe` True

    it "should compute the right number of pitches" $ do
        pitches <- liftIO $ trackFileToList a4File
        length pitches `shouldBe` ceiling (sampleRate * 5 / fromIntegral defaultSampleNum)

a4File :: FilePath
a4File = "test/files/a4-5sec.raw"

sampleRate :: Double
sampleRate = 44100
