{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE JavaScriptFFI        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           Data.Vector.Storable   as V
import           Data.Word              (Word32)
import           Foreign.Ptr
import           Graphics.Glucose

#ifdef ghcjs_HOST_OS
import           GHCJS.Marshal
import           GHCJS.Types            (JSVal)
import           Graphics.Glucose.WebGL
import           JavaScript.TypedArray  as TA

vec :: Vector Float
vec = fromList [0 .. 10]

vecu32 :: Vector Word32
vecu32 = fromList [0 .. 10]

main :: IO ()
main = do
  let (WebGL GLES{..}) = webgl undefined
  floatarray <- withFloatArray vec return
  printF32JS floatarray
  --vec2 <- fromFloatArray floatarray
  --print vec2
  --print $ vec2 == vec

#else

main :: IO ()
main = putStrLn "ghc"

#endif
