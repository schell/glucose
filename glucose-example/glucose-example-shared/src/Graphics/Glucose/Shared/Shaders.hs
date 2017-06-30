{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Glucose.Shared.Shaders
  ( vertex
  , fragment
  ) where

import           Graphics.Gristle
import           Prelude          hiding (return, (>>), (>>=))

vertex
  :: forall ctx. HasContext ctx
  => IxShader ctx '[] '[ In      "vec2" "position"
                       , In      "vec4" "color"
                       , Uniform "mat4" "projection"
                       , Uniform "mat4" "modelview"
                       , Out     "vec4" "fcolor"
                       , Out     "vec4" "gl_Position"
                       ] ()
vertex = do
  pos    <- in_ @"vec2" @"position"
  color  <- in_ @"vec4" @"color"

  proj   <- uniform_ @"mat4" @"projection"
  modl   <- uniform_ @"mat4" @"modelview"

  fcolor <- out_ @"vec4" @"fcolor"
  glPos  <- gl_Position

  main_ $ do
    fcolor .= color
    glPos  .= proj .* modl .* mkvec4 (x pos) (y pos) (f 0.0) (f 1.0)

fragment
  :: forall ctx. (HasContext ctx, KnownSymbol (GLFragName ctx))
  => IxShader ctx '[] '[ In  "vec4" "fcolor"
                       , Out "vec4" (GLFragName ctx)
                       ] ()
fragment = do
  fcolor <- in_ @"vec4" @"fcolor"
  glFrag <- gl_FragColor
  main_ $ glFrag .= fcolor
