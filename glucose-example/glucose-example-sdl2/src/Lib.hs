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
import           Linear
import           SDL                        hiding (OpenGL, Vector,
                                             glBindTexture)
import           System.Exit                (exitFailure)

import           Graphics.Glucose
import           Graphics.Glucose.OpenGL
import           Graphics.Glucose.Shared


runExample :: IO ()
runExample = do
  let cfg = defaultWindow{ windowOpenGL    = Just defaultOpenGL{glProfile = Core Debug 3 3}
                         , windowResizable = True
                         , windowHighDPI   = True
                         }
      gl@(OpenGL GLES{..}) = opengl
  initializeAll
  window <- createWindow "Glucose SDL2 Example" cfg
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
