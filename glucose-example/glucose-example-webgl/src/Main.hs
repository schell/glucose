{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Control.Monad                     (forever, msum)
import           Data.Function                     (fix)
import           GHCJS.DOM                         (currentDocument, syncPoint)
import           GHCJS.DOM.Document                (createElement, getBody)
import           GHCJS.DOM.Element                 (setAttribute)
import           GHCJS.DOM.HTMLCanvasElement       (getContext)
import           GHCJS.DOM.Node                    (appendChild_)
import           GHCJS.DOM.Types                   hiding
                                                    (WebGLRenderingContextBase)
import           JSDOM.Types                       (WebGLRenderingContextBase (..))
import           Language.Javascript.JSaddle.Monad (askJSM, runJSM)

import           Graphics.Glucose
import           Graphics.Glucose.Shared
import           Graphics.Glucose.WebGL


main :: IO ()
main = do
  Just doc  <- currentDocument
  Just body <- getBody doc

  canvas <-
    uncheckedCastTo HTMLCanvasElement <$> createElement doc "canvas"
  appendChild_ body canvas
  setAttribute canvas "width" "640"
  setAttribute canvas "height" "480"
  let ctxStrings = ["webgl", "experimental-webgl"]
      getCtxs str = putStrLn str >> getContext canvas str ([] :: [JSString])
  msum (map getCtxs ctxStrings) >>= \case
    Nothing   -> fail "Could not create rendering context."
    Just (RenderingContext val) ->
      initWebGL (WebGLRenderingContextBase val) >>= \case
        Left err -> fail err
        Right gl@(WebGL GLES{..}) -> makeProgram gl >>= \case
          Left err -> fail err
          Right program -> do
            glClearColor 0 0 0 1
            (vao, projectionLoc, modelviewLoc) <- setup gl program
            ($ 0) $ fix $ \loop t -> do
              drawScene gl vao projectionLoc modelviewLoc 640 480 t
              loop $ t + 0.05
