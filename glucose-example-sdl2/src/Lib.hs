{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Lib
  ( runExample
  ) where

import           Control.Monad              (forever, void)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either (EitherT (..), runEitherT)
import           Data.Bits                  ((.|.))
import           Data.Foldable              (Foldable)
import qualified Data.Foldable              as F
import           Data.IORef
import           Data.Vector.Storable       (Storable, Vector)
import qualified Data.Vector.Storable       as V
import           Foreign.Storable           (sizeOf)
import           Graphics.Glucose
import           Graphics.Glucose.OpenGL    (OpenGL (..), opengl)
import           Graphics.Glucose.Utils     (compileProgram, compileShader)
import           Linear
import           SDL                        hiding (OpenGL, Vector,
                                             glBindTexture)
import           System.Exit                (exitFailure)

vertexShaderSource :: String
vertexShaderSource = unlines
  ["#version 330 core"
  ,"in vec2 position;"
  ,"in vec4 color;"
  ,"out vec4 fcolor;"
  ,"void main () {"
  ,"  gl_Position = vec4(position, 0.0, 1.0);"
  ,"  fcolor = color;"
  ,"}"
  ]

fragmentShaderSource :: String
fragmentShaderSource = unlines
  ["#version 330 core"
  ,"in vec4 fcolor;"
  ,"out vec4 fragColor;"
  ,"void main () {"
  ,"  fragColor = fcolor;"
  ,"}"
  ]

positions :: Vector (V2 Float)
positions = V.fromList [V2 0 0.5, V2 0.5 (-0.5), V2 (-0.5) (-0.5)]

colors :: Vector (V4 Float)
colors = V.fromList [V4 1 0 0 1, V4 0 1 0 1, V4 0 0 1 1]

flatten :: (Foldable f, Storable (f a), Storable a) => Vector (f a) -> Vector a
flatten = V.concatMap (V.fromList . F.toList)

hoist :: Monad m => m (Either e a) -> EitherT e m a
hoist = EitherT

clearError :: (MonadIO (M a), IsGLES a) => a -> String -> (M a) ()
clearError gl msg = do
  let GLES{..} = gles gl
  err <- glGetError
  let msg2 = concat ["(", show err, ") at ", msg]
  maybe (return ()) (liftIO . putStrLn . unwords . (:[msg2])) $ case () of
    () | err == fromIntegral false   -> Nothing
       | err == gl_INVALID_ENUM      -> Just "INVALID_ENUM"
       | err == gl_INVALID_VALUE     -> Just "INVALID_VALUE"
       | err == gl_INVALID_OPERATION -> Just "INVALID_OPERATION"
       | otherwise                   -> Just $ show err

makeProgram :: forall a. IsGLES a => a -> (M a) (Either String (GLProgram a))
makeProgram gl = do
  let GLES{..} = gles gl
  --glEnable gl_DEPTH_TEST
  runEitherT $ do
    vshader <- hoist $ compileShader gl gl_VERTEX_SHADER vertexShaderSource
    fshader <- hoist $ compileShader gl gl_FRAGMENT_SHADER fragmentShaderSource
    program <- hoist $ compileProgram gl [(0, "position"), (1, "color")] [vshader, fshader]
    lift $ do
      glUseProgram program
      mapM_ glDeleteShader [vshader, fshader]
    return program

setupGeometry
  :: forall a.
     ( IsGLES a
     , BufferableData (M a) (Vector Float) (GLBufferabledata a)
     , MonadIO (M a)
     )
  => a
  -> (M a) (GLVertexArrayObject a, GLBuffer a, GLBuffer a)
setupGeometry gl = do
  let GLES{..} = gles gl
  -- Create a new VAO to hold our array settings
  vao <- glCreateVertexArray
  glBindVertexArray vao

  -- Create, enable and fill the position buffer
  posBuffer <- glCreateBuffer
  glBindBuffer gl_ARRAY_BUFFER posBuffer
  glBufferData gl_ARRAY_BUFFER (flatten positions) gl_STATIC_DRAW
  glVertexAttribPointer 0 2 gl_FLOAT false 0 0
  glEnableVertexAttribArray 0
  clearError gl "setupGeometry.setupPositionStuff"

  -- Create, enable and fill the color buffer
  colorBuffer <- glCreateBuffer
  glBindBuffer gl_ARRAY_BUFFER colorBuffer
  glBufferData gl_ARRAY_BUFFER (flatten colors) gl_STATIC_DRAW
  glVertexAttribPointer 1 4 gl_FLOAT false 0 0
  glEnableVertexAttribArray 1
  clearError gl "setupGeometry.setupColorStuff"

  glBindVertexArray noVertexArray
  return (vao, posBuffer, colorBuffer)

drawScene
  :: (MonadIO (M a), IsGLES a)
  => a
  -> GLVertexArrayObject a
  -> (M a) ()
drawScene gl vao = do
  let GLES{..} = gles gl
  glClear $ fromIntegral gl_COLOR_BUFFER_BIT
  glBindVertexArray vao
  glDrawArrays gl_TRIANGLES 0 3
  clearError gl "drawScene.glDrawArrays"
  glBindVertexArray noVertexArray

runExample :: IO ()
runExample = do
  let cfg = defaultWindow{ windowOpenGL    = Just defaultOpenGL{glProfile = Core Debug 3 3}
                         , windowResizable = True
                         , windowHighDPI   = True
                         }
      gl@(OpenGL GLES{..}) = opengl
  initializeAll
  window <- createWindow "Glucose Example" cfg
  void $ glCreateContext window -- from sdl2
  makeProgram gl >>= \case
    Left err -> putStrLn err >> exitFailure
    Right _ -> do
      glClearColor 0 0 0 1
      (vao, _, _) <- setupGeometry gl
      forever $ do
        void pollEvents
        drawScene gl vao
        glSwapWindow window
