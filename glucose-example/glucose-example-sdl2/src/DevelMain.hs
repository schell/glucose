{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Codec.Wavefront              (WavefrontOBJ (..), fromFile)
import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async     (asyncBound, withAsyncBound)
import           Control.Concurrent.STM.TMVar
import           Control.Monad                (mapM_, unless, void)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.STM            (atomically)
import           Control.Monad.Trans          (lift)
import           Control.Monad.Trans.Either   (EitherT (..), runEitherT)
import           Data.Function                (fix)
import           Data.Maybe                   (catMaybes)
import           Data.Vector.Storable         (Vector, fromList)
import           Halive.Utils
import           Linear
import           SDL                          hiding (GLContext, OpenGL, Vector,
                                               glBindTexture)
import           SDL.Video.OpenGL             (glGetDrawableSize)
import           System.Exit                  (exitFailure)

import           Graphics.Glucose
import           Graphics.Glucose.OpenGL
import           Graphics.Glucose.Shared
import           Graphics.Glucose.Utils       (compileProgram, compileShader)

import           Graphics.IxShader            (Binding (..), GLContext (..),
                                               HasContext (..),
                                               HasShaderType (..), IxFragment,
                                               IxShader, IxVertex,
                                               ShaderType (..), getCtx, onlySrc)

import           Shaders
import           Shaders.Simple3d

------------------------------------------------------------------------------
-- IxShader helpers
------------------------------------------------------------------------------
compileIxShader
  :: forall (ctx :: GLContext) (shadertype :: ShaderType) m a j x.
     ( HasContext ctx
     , HasShaderType shadertype
     , CommonGLConstraints a
     , MonadIO m
     )
  => GLES m a
  -> IxShader shadertype ctx '[] j x
  -> m (Either String (GLShader a))
compileIxShader gl@GLES{..} ixshader = runEitherT $ do
  let src0 = onlySrc ixshader
      src1 = case getCtx @ctx of
        OpenGLContext -> "#version 330 core\n" ++ src0
        WebGLContext  -> src0
      shtyp = case getShaderType @shadertype of
        VertexShader   -> gl_VERTEX_SHADER
        FragmentShader -> gl_FRAGMENT_SHADER
  liftIO $ putStrLn $ "\n" ++ src1
  EitherT $ compileShader gl shtyp src1

compileIxProgram
  :: forall (ctx :: GLContext) m a vs j x y.
     ( Binding vs [Maybe String]
     , CommonGLConstraints a
     , HasContext ctx
     , MonadIO m
     )
  => GLES m a
  -> IxVertex   ctx '[] (vs :: [*]) x
  -> IxFragment ctx '[] j y
  -> m (Either String (GLProgram a))
compileIxProgram gl@GLES{..} ixvertex ixfragment = runEitherT $ do
  vshader <- EitherT $ compileIxShader gl ixvertex
  fshader <- EitherT $ compileIxShader gl ixfragment
  let attribNames = catMaybes $ getVertexBinding @vs
      attribLocs  = [0 ..]
      attribs     = zip attribLocs attribNames
  program <- EitherT $ compileProgram gl attribs [vshader, fshader]
  lift $ do
    glUseProgram program
    mapM_ glDeleteShader [vshader, fshader]
  return program

setupAttributes
  :: forall m a.
     ( MonadIO m
     , CommonGLConstraints a
     )
  => GLES m a
  -> GLProgram a
  -> m (GLVertexArrayObject a)
setupAttributes gl@GLES{..} program = do
  -- Create a new VAO to hold our array settings
  vao <- glCreateVertexArray
  glBindVertexArray vao

  -- Create, enable and fill the position buffer
  posBuffer <- glCreateBuffer
  glBindBuffer gl_ARRAY_BUFFER posBuffer
  let geom :: Vector (V2 Float)
      geom = fromList [V2 (-1) (-1), V2 1 (-1), V2 1 1, V2 (-1) 1, V2 (-1) (-1)]
  glBufferData gl_ARRAY_BUFFER (flatten geom) gl_STATIC_DRAW
  glVertexAttribPointer 0 2 gl_FLOAT false 0 0
  glEnableVertexAttribArray 0

  return vao


--setupObjAttributes
--  :: forall a. (IsGLES a, MonadIO (M a))
--  => a
--  -> GLProgram a
--  -> WavefrontOBJ
--  -> (M a) (GLVertexArrayObject a)
--setupObjAttributes gl program obj = do
--  let GLES{..} = gles gl
--  vao <_ glCreateVertexArray
--  glBindVertexArray vao
--
--  posBuffer <- glCreateBuffer
--  glBindBuffer gl_ARRAY_BUFFER posBuffer
--  let geom :: Vector ()


areQuit :: Event -> Bool
areQuit (Event _ QuitEvent) = True
areQuit _                   = False


main :: IO ()
main = do
  window <- reacquire "sdl-window" $ do
    initializeAll
    let ogl = defaultOpenGL{ glProfile = Core Debug 3 3 }
        cfg = defaultWindow{ windowOpenGL      = Just ogl
                           , windowResizable   = True
                           , windowHighDPI     = False
                           , windowInitialSize = V2 640 480
                           }
    window <- createWindow "Glucose SDL2 Example" cfg
    void $ glCreateContext window -- from sdl2
    return window

  obj <- fromFile "/Users/schell/Desktop/torus.obj" >>= \case
    Left err  -> putStrLn err >> exitFailure
    Right obj -> return obj

  let gl@GLES{..} = opengl
  compileIxProgram @'OpenGLContext gl diffuseVertex diffuseFragment >>= \case
    Left err -> fix $ \loop -> do
      putStrLn err
      threadDelay $ 5 * 1000000
      loop
    Right program -> do
      glClearColor 0 0 0 1
      glEnable gl_DEPTH_TEST
      glDepthFunc gl_LESS
      vao         <- setupAttributes gl program --obj
      uTime       <- glGetUniformLocation program "u_time"
      uResolution <- glGetUniformLocation program "u_resolution"
      fix $ \loop -> do
        shouldQuit <- any areQuit <$> pollEvents
        millis     <- ticks
        sz         <- glGetDrawableSize window
        let V2 w h = fromIntegral <$> sz
            t      = fromIntegral millis / 1000
        glUniform1f uTime t
        glUniform2f uResolution w h
        glClear $ fromIntegral gl_COLOR_BUFFER_BIT
        glViewport 0 0 (fromIntegral $ floor w) (fromIntegral $ floor h)
        glBindVertexArray vao
        glDrawArrays gl_TRIANGLE_FAN 0 4
        glSwapWindow window
        unless shouldQuit loop
