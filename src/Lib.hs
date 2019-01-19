{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  ) where

import Control.Monad

import Control.Monad.Loops
import qualified Data.ByteString as B
import Foreign.Marshal.Array
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import System.Exit (exitFailure)
import System.IO

someFunc :: IO ()
someFunc = do
  GLFW.setErrorCallback (Just errorCallback)
  True <- GLFW.init
  GLFW.windowHint $ GLFW.WindowHint'DepthBits (Just 16)
  Just window <- GLFW.createWindow 800 600 "cars" Nothing Nothing
  GLFW.makeContextCurrent (Just window)
  shader1 <- loadShader1
  GL.currentProgram $= Just shader1

  b <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just b
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled

  whileM_ (not <$> GLFW.windowShouldClose window) $ do
    withArrayLen [0.3 :: Float, 0.0, 0.7, 2.0, 0.9, 5.0] $ \count arr ->
      GL.bufferData GL.ArrayBuffer $= (fromIntegral count * 4, arr, GL.StreamDraw)
    GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 1 GL.Float 8 nullPtr)
    GL.vertexAttribPointer (GL.AttribLocation 1) $= (GL.ToFloat, GL.VertexArrayDescriptor 1 GL.Float 8 (plusPtr nullPtr 4))
    GL.drawArrays GL.Points 0 3
    GLFW.swapBuffers window
    GLFW.pollEvents
  GLFW.destroyWindow window
  GLFW.terminate

errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

loadShader1 :: IO GL.Program
loadShader1 = do
  text <- B.readFile "assets/shaders/shader1.glsl"
  vert <- makeShader GL.VertexShader text
  geom <- makeShader GL.GeometryShader text
  frag <- makeShader GL.FragmentShader text
  program <- GL.createProgram
  mapM_ (GL.attachShader program) [vert, geom, frag]
  GL.linkProgram program
  linked <- GL.get (GL.linkStatus program)
  GL.validateProgram program
  valid <- GL.get (GL.validateStatus program)
  unless (linked && valid) $ GL.get (GL.programInfoLog program) >>= logAndExit
  return program

makeShader :: GL.ShaderType -> B.ByteString -> IO GL.Shader
makeShader shaderType text = do
  s <- GL.createShader shaderType
  GL.shaderSourceBS s $=
    B.concat ["#version 330 core\n#define ", GL.packUtf8 (show shaderType), "\n", text]
  GL.compileShader s
  success <- GL.get (GL.compileStatus s)
  unless success $ GL.get (GL.shaderInfoLog s) >>= logAndExit
  return s

logAndExit :: String -> IO ()
logAndExit s = hPutStrLn stderr s >> exitFailure
