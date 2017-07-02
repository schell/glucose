{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RebindableSyntax #-}
module Shaders where

import           Graphics.Gristle
import           Prelude          hiding (return, (>>), (>>=))

passthruVertex
  :: forall (ctx :: GLContext). HasContext ctx
  => IxShader ctx '[] '[ In "vec2" "position"
                       , Out "vec4" "gl_Position"
                       ] ()
passthruVertex = do
  p <- in_
  glpos <- gl_Position
  main_ $ glpos .= mkvec4 (x p) (y p) (f 0) (f 1)

helloFrag
  :: forall (ctx :: GLContext). (HasContext ctx, KnownSymbol (GLFragName ctx))
  => IxShader ctx '[] '[ Out "vec4" (GLFragName ctx) ] ()
helloFrag = do
  frag <- gl_FragColor
  main_ $ frag .= mkvec4 (f 1) (f 0) (f 0) (f 1)
