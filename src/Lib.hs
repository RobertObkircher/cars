{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( someFunc,
  ) where

import Control.Comonad
import Control.Concurrent
import Data.Foldable
import Control.Monad
import Control.Monad.Loops
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Sequence as S
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
  state <- newIORef makeStreet
  numCellsUniformLocation <- GL.get $ GL.uniformLocation shader1 "numCells"
  let numCellsUniform = GL.uniform numCellsUniformLocation :: GL.StateVar GL.GLfloat
  whileM_ (not <$> GLFW.windowShouldClose window) $ do
    GL.clear [GL.ColorBuffer]
    street <- readIORef state
    let numCells = length (before street) + length (after street) + 1
    numCellsUniform $= fromIntegral numCells
    let verts = toVertList street
    withArrayLen verts $ \count arr ->
      GL.bufferData GL.ArrayBuffer $=
      (fromIntegral count * 4, arr, GL.StreamDraw)
    GL.vertexAttribPointer (GL.AttribLocation 0) $=
      (GL.ToFloat, GL.VertexArrayDescriptor 1 GL.Float 8 nullPtr)
    GL.vertexAttribPointer (GL.AttribLocation 1) $=
      (GL.ToFloat, GL.VertexArrayDescriptor 1 GL.Float 8 (plusPtr nullPtr 4))
    GL.drawArrays GL.Points 0 (fromIntegral (length verts) `div` 2)
    GLFW.swapBuffers window
    threadDelay (1000 * 1000)
    writeIORef state (update street)
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
    B.concat
      ["#version 330 core\n#define ", GL.packUtf8 (show shaderType), "\n", text]
  GL.compileShader s
  success <- GL.get (GL.compileStatus s)
  unless success $ GL.get (GL.shaderInfoLog s) >>= logAndExit
  return s

logAndExit :: String -> IO ()
logAndExit s = hPutStrLn stderr s >> exitFailure

data Cell
  = Car Int -- ^ speed
  | Empty
  deriving (Eq, Show)

data Universe a = Universe
  { before :: [a]
  , focus :: a
  , after :: [a]
  } deriving (Eq, Show)

makeStreet :: Universe Cell
-- makeStreet = Universe [] (Car 4) [Empty, Empty, Car 1, Empty, Empty, Empty, Car 0, Car 0, Empty, Empty]
-- makeStreet = Universe [] (Car 4) [Empty, Empty, Empty, Empty, Empty, Empty, Empty]
makeStreet = Universe [] (Car 1) (replicate 5 Empty)

toVertList :: Universe Cell -> [Float]
toVertList Universe{..} = concat $ zipWith toVert list [0..]
  where
    list = before ++ focus:after
    l = fromIntegral $ length list
    toVert :: Cell -> Int -> [Float]
    toVert Empty _ = []
    toVert (Car speed) index = [fromIntegral index / l, fromIntegral speed]

prev :: Universe a -> Universe a
prev (Universe [] f a) = Universe b newFocus [f]
  where
    (newFocus:b) = reverse a
prev (Universe (newFocus:b) f a) = Universe b newFocus (f : a)

next :: Universe a -> Universe a
next (Universe b f []) = Universe [f] newFocus a
  where
    (newFocus:a) = reverse b
next (Universe b f (newFocus:a)) = Universe (f : b) newFocus a

instance Functor Universe where
  fmap f Universe {..} =
    Universe {before = fmap f before, focus = f focus, after = fmap f after}

instance Comonad Universe where
  extract Universe {..} = focus
  duplicate z = Universe
    { before = toList $ S.iterateN (length (before z)) prev (prev z)
    , focus = z
    , after = toList $ S.iterateN (length (after z)) next (next z)
    }

-- https://theory.org/complexity/traffic/
update x = x =>> acceleration =>> slowingDown =>> vehicleMotion

maxSpeed :: Int
maxSpeed = 1

acceleration :: Universe Cell -> Cell
acceleration u = case extract u of
  Empty -> Empty
  Car speed -> Car $ min (speed + 1) maxSpeed

gap :: Universe Cell -> Int -> Int
gap _ dist | dist > maxSpeed = dist
gap u dist = case extract u of
  Empty -> gap (next u) (dist + 1)
  Car _ -> dist

slowingDown :: Universe Cell -> Cell
slowingDown u = case extract u of
  Empty -> Empty
  Car speed -> Car $ min (gap (next u) 0) speed

vehicleMotion :: Universe Cell -> Cell
vehicleMotion = vehicleMotion' 0

vehicleMotion' :: Int -> Universe Cell -> Cell
vehicleMotion' dist _ | dist > maxSpeed = Empty
vehicleMotion' dist u = case extract u of
  c@(Car speed) | speed == dist -> c
  _ -> vehicleMotion' (dist + 1) (prev u)
