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

module Main where

import           Control.Monad              (forever, unless, void)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either (EitherT (..), runEitherT)
import           Data.Bits                  ((.|.))
import           Data.Foldable              (Foldable)
import qualified Data.Foldable              as F
import           Data.Function              (fix)
import           Data.IORef
import           Data.Vector.Storable       (Storable, Vector)
import qualified Data.Vector.Storable       as V
import           Foreign.Storable           (sizeOf)
import           Linear
import           SDL                        hiding (OpenGL, Vector,
                                             glBindTexture)
import           SDL.Raw.Event              (quitRequested)
import           SDL.Video.OpenGL           (glGetDrawableSize)
import           System.Exit                (exitFailure)

import           Graphics.Glucose
import           Graphics.Glucose.OpenGL
import           Graphics.Glucose.Shared


areQuit :: Event -> Bool
areQuit (Event _ QuitEvent) = True
areQuit _                   = False

main :: IO ()
main = do
  initializeAll
  let cfg = defaultWindow{ windowOpenGL    = Just defaultOpenGL{glProfile = Core Debug 3 3}
                         , windowResizable = True
                         , windowHighDPI   = True
                         }
  window <- createWindow "Glucose SDL2 Example" cfg
  void $ glCreateContext window -- from sdl2
  let gl@(OpenGL GLES{..}) = opengl
  makeProgram gl >>= \case
    Left err -> putStrLn err >> exitFailure
    Right program -> do
      glClearColor 0 0 0 1
      (vao, projectionLoc, modelviewLoc) <- setup gl program
      ($ 0) $ fix $ \loop t -> do
        shouldQuit <- any areQuit <$> pollEvents
        sz <- glGetDrawableSize window
        let V2 w h = fromIntegral <$> sz
        drawScene gl vao projectionLoc modelviewLoc w h t
        glSwapWindow window
        unless shouldQuit $ loop (t + 0.05)
