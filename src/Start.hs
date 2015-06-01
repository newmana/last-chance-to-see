{-# LANGUAGE PackageImports #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Juicy
import Control.Concurrent (threadDelay)
import Control.Monad (when)

nextWindow :: Maybe Window -> (Window -> IO a) -> Window -> IO ()
nextWindow m f win = do
    GLFW.makeContextCurrent m
    _ <- f win
    GLFW.setErrorCallback $ Just simpleErrorCallback
    GLFW.destroyWindow win

simpleErrorCallback :: (Show a1, Show a) => a -> a1 -> IO ()
simpleErrorCallback e s =
    putStrLn $ unwords [show e, show s]

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        maybe (return ()) (nextWindow m f) m
        GLFW.terminate

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed = True
isPress KeyState'Repeating = True
isPress _ = False

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
          displayPicture (640, 480) white glossState 1.0 $ texture
          swapBuffers window
          k <- keyIsPressed window Key'Escape
          if k
            then return()
            else loop glossState window texture

