{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
module Graphics.Glucose.Utils where

import           Control.Monad    (forM_)
import           Graphics.Glucose


compileShader
  :: forall m (a :: GLBackend). (Monad m, Eq (GLBoolean a))
  => GLES m a
  -> GLEnum a
  -> String
  -> m (Either String (GLShader a))
compileShader GLES{..} shtype src = do
  s <- glCreateShader shtype
  glShaderSource s src
  glCompileShader s
  glGetShaderParameter s gl_COMPILE_STATUS >>= \case
    Right _ -> return $ Left "Glucose library error, this should never happen."
    Left success
      | success == false -> Left <$> glGetShaderInfoLog s
      | otherwise -> return $ Right s


compileProgram
  :: forall m (a :: GLBackend). (Monad m, Eq (GLBoolean a))
  => GLES m a
  -> [(GLUint a, String)]
  -> [GLShader a]
  -> m (Either String (GLProgram a))
compileProgram GLES{..} attribs shaders = do
  program <- glCreateProgram
  forM_ shaders $ glAttachShader program
  forM_ attribs $ uncurry $ glBindAttribLocation program
  glLinkProgram program
  glGetProgramParameter program gl_LINK_STATUS >>= \case
    Left status
      | status == true -> return $ Right program
      | otherwise -> Left <$> glGetProgramInfoLog program
    _ -> return $ Left "Glucose library error, this should never happen."
