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

module DevelMain where

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

import           Graphics.Gristle             (Binding (..), GLContext (..),
                                               HasContext (..), IxShader,
                                               getCtx, ixShaderSrc)

import           Shaders

------------------------------------------------------------------------------
-- IxShader helpers
------------------------------------------------------------------------------
compileIxShader
  :: forall (ctx :: GLContext) a j x. (HasContext ctx, MonadIO (M a), IsGLES a)
  => a
  -> IxShader ctx '[] j x
  -> GLEnum a
  -> (M a) (Either String (GLShader a))
compileIxShader gl ixshader shadertype = runEitherT $ do
  let GLES{..} = gles gl
  src0 <- EitherT $ return $ ixShaderSrc ixshader
  let src1 = case getCtx @ctx of
        OpenGLContext -> "#version 330 core\n" ++ src0
        WebGLContext  -> src0
  liftIO $ putStrLn $ "\n" ++ src1
  EitherT $ compileShader gl shadertype src1

compileIxProgram
  :: forall (ctx :: GLContext) a vs j x y.
     ( IsGLES a
     , Binding vs [Maybe String]
     , Enum (GLUint a)
     , HasContext ctx
     , MonadIO (M a)
     )
  => a
  -> IxShader ctx '[] (vs :: [*]) x
  -> IxShader ctx '[] j y
  -> (M a) (Either String (GLProgram a))
compileIxProgram gl ixvertex ixfragment = runEitherT $ do
  let GLES{..} = gles gl
  vshader <- EitherT $ compileIxShader gl ixvertex gl_VERTEX_SHADER
  fshader <- EitherT $ compileIxShader gl ixfragment gl_FRAGMENT_SHADER
  let attribNames = catMaybes $ getVertexBinding @vs
      attribLocs  = [0 ..]
      attribs     = zip attribLocs attribNames
  program <- EitherT $ compileProgram gl attribs [vshader, fshader]
  lift $ do
    glUseProgram program
    mapM_ glDeleteShader [vshader, fshader]
  return program

setupAttributes
  :: forall a. (IsGLES a, MonadIO (M a))
  => a
  -> GLProgram a
  -> (M a) (GLVertexArrayObject a)
setupAttributes gl program = do
  let GLES{..} = gles gl
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

  let gl@(OpenGL GLES{..}) = opengl
  compileIxProgram @'OpenGLContext gl passthruVertex frag031 >>= \case
    Left err -> fix $ \loop -> do
      putStrLn err
      threadDelay $ 5 * 1000000
      loop
    Right program -> do
      glClearColor 0 0 0 1
      vao   <- setupAttributes gl program
      uTime <- glGetUniformLocation program "u_time"
      fix $ \loop -> do
        shouldQuit <- any areQuit <$> pollEvents
        millis     <- ticks
        sz         <- glGetDrawableSize window
        let V2 w h = fromIntegral <$> sz
            t      = fromIntegral millis / 1000
        glUniform1f uTime t
        glClear $ fromIntegral gl_COLOR_BUFFER_BIT
        glViewport 0 0 (fromIntegral $ floor w) (fromIntegral $ floor h)
        glBindVertexArray vao
        glDrawArrays gl_TRIANGLE_FAN 0 4
        glSwapWindow window
        unless shouldQuit loop
