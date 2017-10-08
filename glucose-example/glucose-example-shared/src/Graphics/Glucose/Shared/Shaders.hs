{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Graphics.Glucose.Shared.Shaders
  ( myvertex
  , myfragment
  ) where

import           Graphics.IxShader

myvertex
  :: forall (ctx :: GLContext). HasContext ctx
  => IxShader ctx '[] '[ In      Xvec2 "position"
                       , In      Xvec4 "color"
                       , Uniform Xmat4 "projection"
                       , Uniform Xmat4 "modelview"
                       , Out     Xvec4 "fcolor"
                       , Out     Xvec4 "gl_Position"
                       , Main
                       ] ()
myvertex = do
  pos    <- in_
  color  <- in_
  proj   <- uniform_
  modl   <- uniform_
  fcolor <- out_
  glPos  <- gl_Position
  main_ $ do
    fcolor .= color
    glPos  .= proj .* modl .* (pos .: 0.0 .: 1.0)

myfragment
  :: forall (ctx :: GLContext). IsGLContext ctx
  => IxShader ctx '[] '[ In  Xvec4 "fcolor"
                       , Out Xvec4 (GLFragName ctx)
                       , Main
                       ] ()
myfragment = do
  fcolor <- in_
  glFrag <- gl_FragColor
  main_ $ glFrag .= fcolor
