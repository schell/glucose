{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE TypeApplications  #-}
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
call :: forall (t :: Symbol). String -> SocketReadOnly t -> SocketReadOnly t
call fncstr (Socket a) = Socket $ concat [fncstr, "(", a, ")"]

call2 :: forall (t :: Symbol). String -> SocketReadOnly t -> SocketReadOnly t -> SocketReadOnly t
call2 fncstr (Socket a) (Socket b) = Socket $ concat [fncstr, "(", a, ",", b, ")"]

callInfix :: forall (t :: Symbol). String -> SocketReadOnly t -> SocketReadOnly t -> SocketReadOnly t
callInfix fncstr (Socket a) (Socket b) = Socket $ concat [a, fncstr, b]

instance Num (SocketReadOnly (t :: Symbol)) where
  (+) = call2 "+"
  (-) = call2 "-"
  (*) = call2 "*"
  negate (Socket a) = Socket $ concat ["(-", a, ")"]
  abs    = call "abs"
  signum = call "sign"
  fromInteger = Socket . show . (fromInteger :: Integer -> Float)

instance Fractional (SocketReadOnly (t :: Symbol)) where
  fromRational a = Socket $ show $
    (fromIntegral (numerator a) :: Float) / fromIntegral (denominator a)
  (/) = callInfix "/"

instance Floating (SocketReadOnly (t :: Symbol)) where
  pi = Socket $ show (pi :: Float)
  exp  = call @"float" "exp"
  log  = call "log"
  sqrt = call "sqrt"
  (**) = call2 "pow"
  logBase a b = log b / log a
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

timeFrag
  :: forall (ctx :: GLContext). (HasContext ctx, KnownSymbol (GLFragName ctx))
  => IxShader ctx '[] '[ Uniform "float" "u_time"
                       , Out "vec4" (GLFragName ctx)
                       ] ()
timeFrag = do
  u_time <- uniform_
  frag <- gl_FragColor
  main_ $ frag .= mkvec4 (abs $ sin u_time) (f 0) (f 0) (f 1)
