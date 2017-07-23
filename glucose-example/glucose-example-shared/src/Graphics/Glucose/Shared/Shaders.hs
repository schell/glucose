{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Glucose.Shared.Shaders
  ( myvertex
  , myfragment
  ) where

import           Graphics.Gristle
import           Prelude          hiding (return, (>>), (>>=))

myvertex
  :: forall ctx. HasContext ctx
  => IxShader ctx '[] '[ In      "vec2" "position"
                       , In      "vec4" "color"
                       , Uniform "mat4" "projection"
                       , Uniform "mat4" "modelview"
                       , Out     "vec4" "fcolor"
                       , Out     "vec4" "gl_Position"
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
    glPos  .= proj .* modl .* mkvec4 (x pos) (y pos) (f 0.0) (f 1.0)

myfragment
  :: forall ctx. (HasContext ctx, KnownSymbol (GLFragName ctx))
  => IxShader ctx '[] '[ In  "vec4" "fcolor"
                       , Out "vec4" (GLFragName ctx)
                       , Main
                       ] ()
myfragment = do
  fcolor <- in_
  glFrag <- gl_FragColor
  main_ $ glFrag .= fcolor
