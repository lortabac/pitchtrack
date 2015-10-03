module Test.Simple where

import           PitchTrack.Track

import           System.IO
import           System.Process

testPrint :: IO ()
testPrint = trackFilePrint "pitchtrack/src/Test/note.raw"

testList :: IO [Double]
testList = trackFileToList "pitchtrack/src/Test/note.raw"

testRealTime :: IO ()
testRealTime = withFile "/dev/null" WriteMode $ \devNull -> do
    let recProcess = (shell "rec -e float -b 64 -r 44100 -c 1 -t raw -")
            { std_out = CreatePipe
            , std_err = UseHandle devNull
            , delegate_ctlc = True
            }
    (_, Just recOutHandle, _, _) <- createProcess recProcess
    trackHandlePrint recOutHandle
