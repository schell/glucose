{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- |
-- Copyright:  (c) 2017 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

module DevelMain (update) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.Codensity
import           Control.Monad.IO.Class
import           Graphics.GL.Core33
import           Main
import           Rapid
import qualified SDL                      as Sdl


cBracket :: IO a -> (a -> IO b) -> Codensity IO a
cBracket o c = Codensity (bracket o c)


cBracket_ :: IO a -> IO b -> Codensity IO a
cBracket_ o c = cBracket o (\_ -> c)

mainWith :: Sdl.Window -> (forall a. IO a -> IO a) -> IO ()
mainWith win sdl = go 0
    where
    go ang | ang >= pi = go (ang - 2*pi)
    go ang = do

        let draw = do
                glClearColor (0.5 + 0.5 * sin ang) 0 0 1
                glClear GL_COLOR_BUFFER_BIT
                Sdl.glSwapWindow win

        ev <- sdl Sdl.pollEvent
        case Sdl.eventPayload <$> ev of
          Just Sdl.QuitEvent -> pure ()
          Just _ -> go (ang + 0.01)
          Nothing -> do
              sdl draw
              go (ang + 0.05)


newAppWindow :: Codensity IO Sdl.Window
newAppWindow = do
    cBracket_ (Sdl.initialize [Sdl.InitVideo]) Sdl.quit
    win <- Sdl.createWindow "Test" winCfg
    win <$ Sdl.glCreateContext win

    where
    glCfg = Sdl.defaultOpenGL {
              Sdl.glProfile = Sdl.Core Sdl.Normal 3 3
            }

    winCfg = Sdl.defaultWindow {
               Sdl.windowOpenGL = Just glCfg
             }


update :: IO ()
update =
    rapid 0 $ \r -> do
        sdlVar <- createRef r "sdl-var" newEmptyMVar

        startWith asyncBound r "sdl-win" . lowerCodensity $ do
            win <- newAppWindow
            actionVar <- liftIO newEmptyMVar
            cBracket_ (putMVar sdlVar (win, actionVar)) (takeMVar sdlVar)
            let go = join (takeMVar actionVar) >> go
            liftIO go

        restart r "sdl-app" $ do
            (win, actionVar) <- readMVar sdlVar
            mainWith win $ \c -> do
                resVar <- newEmptyMVar
                putMVar actionVar (c >>= putMVar resVar)
                takeMVar resVar
