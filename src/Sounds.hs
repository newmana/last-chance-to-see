{-# LANGUAGE PackageImports, RecursiveDo #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss hiding (play)
import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.ViewPort
import System.Exit ( exitSuccess, exitFailure )
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless, join)
import Control.Monad.Fix (fix)
import Control.Applicative ((<*>), (<$>))
import FRP.Elerea.Simple as Elerea
import System.Random
import Sound.ALUT hiding (Static, direction)
import System.IO ( hPutStrLn, stderr )
import Data.List (intersperse)

data Sounds = Sounds {
	narration :: Source
}

main :: IO ()
main = do
  withProgNameAndArgs runALUT $ \progName args -> do
  sounds <- loadSounds
  backgroundMusic (narration sounds)
 	
backgroundMusic :: Source -> IO ()
backgroundMusic source = do
  loopingMode source $= Looping
  play [source]

  errs <- get alErrors  
  unless (null errs) $ do
    hPutStrLn stderr (concat (intersperse "," [ d | ALError _ d <- errs ]))
    exitFailure

  let waitWhilePlaying = do
      sleep 0.1
      state <- get (sourceState source)
      when (state == Playing) $ waitWhilePlaying
  waitWhilePlaying

loadSound :: FilePath -> IO Source
loadSound path = do
    buf <- createBuffer (File path)
    source <- genObjectName
    buffer source $= Just buf
    return source

loadSounds :: IO Sounds
loadSounds = do
    narrationSound <- loadSound "/Volumes/LAST_CH_1/CHAPTER1/SECTION1/SOUND.WAV"
    return $ Sounds narrationSound