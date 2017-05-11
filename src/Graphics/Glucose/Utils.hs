{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.Glucose.Utils where

import           Control.Monad    (forM_)
import           Graphics.Glucose

compileShader
  :: forall a. IsGLES a
  => a
  -> GLEnum a
  -> String
  -> (M a) (Either String (GLShader a))
compileShader gl shtype src = do
  let GLES{..} = gles gl
  s <- glCreateShader shtype
  glShaderSource s src
  glCompileShader s
  glGetShaderParameter s gl_COMPILE_STATUS >>= \case
    Right _ -> return $ Left "Glucose library error, this should never happen."
    Left success
      | success == false -> Left <$> glGetShaderInfoLog s
      | otherwise -> return $ Right s

compileProgram
  :: forall a. IsGLES a
  => a
  -> [(GLUint a, String)]
  -> [GLShader a]
  -> (M a) (Either String (GLProgram a))
compileProgram gl attribs shaders = do
  let GLES{..} = gles gl
  program <- glCreateProgram
  forM_ shaders $ glAttachShader program
  forM_ attribs $ uncurry $ glBindAttribLocation program
  glLinkProgram program
  glGetProgramParameter program gl_LINK_STATUS >>= \case
    Left status
      | status == true -> return $ Right program
      | otherwise -> Left <$> glGetProgramInfoLog program
    _ -> return $ Left "Glucose library error, this should never happen."
