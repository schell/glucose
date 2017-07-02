{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}
module Shaders where

import           Data.Ratio       (denominator, numerator)
import           GHC.TypeLits
import           Graphics.Gristle hiding (Fractional, Num)
import           Prelude          hiding (return, (>>), (>>=))

------------------------------------------------------------------------------
-- Stuff goes to mainline gristle
------------------------------------------------------------------------------
gl_FragCoord :: SocketReadOnly "vec4"
gl_FragCoord = Socket "gl_FragCoord"

class IsVec2 (a :: Symbol) where
  type ToVec2 a :: Symbol
instance IsVec2 "vec2" where
  type ToVec2 "vec2" = "vec2"
instance IsVec2 "vec3" where
  type ToVec2 "vec3" = "vec2"
instance IsVec2 "vec4" where
  type ToVec2 "vec4" = "vec2"
instance IsVec2 "ivec2" where
  type ToVec2 "ivec2" = "ivec2"
instance IsVec2 "ivec3" where
  type ToVec2 "ivec3" = "ivec2"
instance IsVec2 "ivec4" where
  type ToVec2 "ivec4" = "ivec2"
instance IsVec2 "bvec2" where
  type ToVec2 "bvec2" = "bvec2"
instance IsVec2 "bvec3" where
  type ToVec2 "bvec3" = "bvec2"
instance IsVec2 "bvec4" where
  type ToVec2 "bvec4" = "bvec2"
instance IsVec2 "uvec2" where
  type ToVec2 "uvec2" = "uvec2"
instance IsVec2 "uvec3" where
  type ToVec2 "uvec3" = "uvec2"
instance IsVec2 "uvec4" where
  type ToVec2 "uvec4" = "uvec2"

xy
  :: forall (t :: Symbol) (w :: Bool). IsVec2 t
  => Socket t 'True w -> Socket (ToVec2 t) 'True w
xy (Socket a) = Socket $ concat ["(", a, ").xy"]
------------------------------------------------------------------------------
-- Book of Shaders
------------------------------------------------------------------------------
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
  main_ $ frag .= mkvec4 (f 1) (f 0) (f 1) (f 1)
--------------------------------------------------------------------------------
-- 03
--------------------------------------------------------------------------------
frag030
  :: forall (ctx :: GLContext). (HasContext ctx, KnownSymbol (GLFragName ctx))
  => IxShader ctx '[] '[ Uniform "float" "u_time"
                       , Out "vec4" (GLFragName ctx)
                       ] ()
frag030 = do
  uTime <- uniform_
  frag <- gl_FragColor
  main_ $ frag .= mkvec4 (abs $ sin uTime) (f 0) (f 0) (f 1)

frag031
  :: forall (ctx :: GLContext). (HasContext ctx, KnownSymbol (GLFragName ctx))
  => IxShader ctx '[] '[ Uniform "vec2" "u_resolution"
                       , Uniform "vec2" "u_mouse"
                       , Uniform "float" "u_time"
                       , Out "vec4" (GLFragName ctx)
                       ] ()
frag031 = do
  uResolution <- uniform_
  _           <- uniform_
  _           <- uniform_
  frag        <- gl_FragColor
  main_ $ do
    let st = xy gl_FragCoord / uResolution
    frag .= mkvec4 (x st) (y st) (f 0) (f 1)
