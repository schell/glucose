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

import           Data.List        (intercalate)
import           GHC.TypeLits
import           Graphics.Gristle
import           Prelude          hiding (return, (>>), (>>=))

------------------------------------------------------------------------------
-- Stuff goes to mainline gristle
------------------------------------------------------------------------------
vec2
  :: forall (ctx :: GLContext) i.
     String
  -> IxShader ctx i i (SocketReadWrite "vec2")
vec2 s = do
  nxt_ $ unwords ["vec2", s, ";"]
  return $ Socket s

class IsR3 (a :: Symbol) where
  type ToR3 a :: Symbol
instance IsR3 "vec2" where
  type ToR3 "vec2" = "float"
instance IsR3 "vec3" where
  type ToR3 "vec3" = "float"
instance IsR3 "vec4" where
  type ToR3 "vec4" = "float"
instance IsR3 "ivec2" where
  type ToR3 "ivec2" = "int"
instance IsR3 "ivec3" where
  type ToR3 "ivec3" = "int"
instance IsR3 "ivec4" where
  type ToR3 "ivec4" = "int"
instance IsR3 "bvec2" where
  type ToR3 "bvec2" = "bool"
instance IsR3 "bvec3" where
  type ToR3 "bvec3" = "bool"
instance IsR3 "bvec4" where
  type ToR3 "bvec4" = "bool"
instance IsR3 "uvec2" where
  type ToR3 "uvec2" = "uint"
instance IsR3 "uvec3" where
  type ToR3 "uvec3" = "uint"
instance IsR3 "uvec4" where
  type ToR3 "uvec4" = "uint"

z
  :: forall (t :: Symbol) (w :: Bool). IsR3 t
  => Socket t 'True w -> Socket (ToR3 t) 'True w
z (Socket s) = Socket $ concat ["(", s, ").z"]

mkvec3
  :: forall (w1 :: Bool) (w2 :: Bool) (w3 :: Bool).
     Socket "float" 'True w1
  -> Socket "float" 'True w2
  -> Socket "float" 'True w3
  -> Socket "vec3" 'True 'False
mkvec3 (Socket a) (Socket b) (Socket c) =
  Socket $ "vec3(" ++ intercalate "," [a, b, c] ++ ")"

call3
  :: forall (t :: Symbol) (r :: Bool) (w :: Bool) (rr :: Bool) (ww :: Bool).
     String
  -> Socket t r w
  -> Socket t r w
  -> Socket t r w
  -> Socket t rr ww
call3 fncstr (Socket a) (Socket b) (Socket c) =
  Socket $ concat [fncstr, "(", a, ",", b, ",", c, ")"]

smoothstep
  :: forall (t :: Symbol) (r :: Bool) (w :: Bool).
     Socket t r w
  -> Socket t r w
  -> Socket t r w
  -> SocketReadOnly t
smoothstep = call3 "smoothstep"

type Function t ps ctx i = ps -> IxShader ctx i i ()

func funcname sh = do
  sub (unwords [retype, funcname, "{"]) "}" sh
  return $ \ps -> do
  where retype = symbolVal $ Proxy @ret
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
  => IxShader ctx '[] '[ Uniform "vec2"  "u_resolution"
                       , Uniform "vec2"  "u_mouse"
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
--------------------------------------------------------------------------------
-- 05
--------------------------------------------------------------------------------
frag050
  :: forall (ctx :: GLContext). (HasContext ctx, KnownSymbol (GLFragName ctx))
  => IxShader ctx '[] '[ Uniform "vec2"  "u_resolution"
                       , Uniform "vec2"  "u_mouse"
                       , Uniform "float" "u_time"
                       , Out "vec4" (GLFragName ctx)
                       ] ()
frag050 = do
  uResolution <- uniform_
  _           <- uniform_
  _           <- uniform_
  frag        <- gl_FragColor

  main_ $ do
    let plot :: SocketReadOnly "vec2" -> SocketReadOnly "float" -> SocketReadOnly "float"
        plot st pct = smoothstep (pct - 0.02) pct (y st)
                      - smoothstep pct (pct + 0.02) (y st)
        st :: SocketReadOnly "vec2"
        st = xy gl_FragCoord / uResolution
        y0 :: SocketReadOnly "float"
        y0 = x st
        color0 :: SocketReadOnly "vec3"
        color0 = mkvec3 y0 y0 y0
        pct :: SocketReadOnly "float"
        pct = plot st y0
        color1 :: SocketReadOnly "vec3"
        color1 = ((f 1.0 - pct) .* color0) + (pct .* mkvec3 (f 0) (f 1) (f 0))
    frag .= mkvec4 (x color1) (y color1) (z color1) (f 1)
