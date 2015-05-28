{-# LANGUAGE PackageImports #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Juicy
import System.Exit ( exitSuccess )
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless)
import Data.Maybe

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
            (Just win) -> do
                GLFW.makeContextCurrent m
                f win
                GLFW.setErrorCallback $ Just simpleErrorCallback
                GLFW.destroyWindow win
            Nothing -> return ()
        GLFW.terminate
    where
        simpleErrorCallback e s =
            putStrLn $ unwords [show e, show s]

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True
isPress _                  = False

-- renderFrame :: Window -> RS.stateInit -> Picture -> IO ()
renderFrame window glossState texture = do
  displayPicture (640, 480) white glossState 1.0 $ texture
  swapBuffers window

main :: IO ()
main = do
    glossState <- initState
    let width = 640
        height = 480
    texture <- loadJuicy "/Volumes/LAST_CH_1/CHAPTER1/640_8/_DNA_AND.DIB"
    withWindow width height "Resurrection" $ \win -> do
        maybe (return ()) (loop glossState win) texture
      where     
        loop glossState window texture = do
          threadDelay 20000
          pollEvents
          renderFrame window glossState texture
          k <- keyIsPressed window Key'Escape
          if k
            then return()
            else loop glossState window texture
