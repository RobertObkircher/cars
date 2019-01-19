module Lib
  ( someFunc
  ) where

import Control.Monad.Loops
import qualified Graphics.UI.GLFW as GLFW
import System.IO

someFunc :: IO ()
someFunc = do
  GLFW.setErrorCallback (Just errorCallback)
  True <- GLFW.init
  GLFW.windowHint $ GLFW.WindowHint'DepthBits (Just 16)
  Just window <- GLFW.createWindow 800 600 "cars" Nothing Nothing
  GLFW.makeContextCurrent (Just window)
  whileM_ (not <$> GLFW.windowShouldClose window) $ do
    GLFW.swapBuffers window
    GLFW.pollEvents
  GLFW.destroyWindow window
  GLFW.terminate

errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr
