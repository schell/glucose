{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}
module Shaders where

import           Graphics.Gristle

------------------------------------------------------------------------------
-- Stuff goes to mainline gristle
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Book of Shaders
------------------------------------------------------------------------------
passthruVertex
  :: forall (ctx :: GLContext). IsGLContext ctx 
  => IxShader ctx '[] '[ In Xvec2 "position"
                       , Out Xvec4 "gl_Position"
                       , Main
                       ] ()
passthruVertex = do
  p     <- in_
  glpos <- gl_Position
  main_ $ glpos .= mkvec4 (x p) (y p) 0.0 1.0

helloFrag
  :: forall (ctx :: GLContext). IsGLContext ctx 
  => IxShader ctx '[] '[ Out Xvec4 (GLFragName ctx)
                       , Main
                       ] ()
helloFrag = do
  frag <- gl_FragColor
  main_ $ frag .= mkvec4 1.0 0.0 1.0 1.0
--------------------------------------------------------------------------------
-- 03
--------------------------------------------------------------------------------
frag030
  :: IsGLContext ctx 
  => IxShader ctx '[] '[ Uniform Xfloat "u_time"
                       , Out Xvec4 (GLFragName ctx)
                       , Main
                       ] ()
frag030 = do
  uTime <- uniform_
  frag <- gl_FragColor
  main_ $ frag .= mkvec4 (abs $ sin uTime) 0.0 0.0 1.0

type RMT = '[ Uniform Xvec2 "u_resolution"
            , Uniform Xvec2 "u_mouse"
            , Uniform Xfloat "u_time"
            ]
type RMTFrag ctx =
  IxShader ctx '[] (RMT :++ '[Out Xvec4 (GLFragName ctx), Main])


frag031
  :: forall (ctx :: GLContext). IsGLContext ctx => RMTFrag ctx () 
frag031 = do
  uResolution <- uniform_
  _           <- uniform_
  _           <- uniform_
  frag        <- gl_FragColor
  main_ $ do
    st <- define $ Xvec2 "st"
    st .= xy gl_FragCoord / xy uResolution
    frag .= mkvec4 (x st) (y st) 0.0 1.0
--------------------------------------------------------------------------------
-- 05
--------------------------------------------------------------------------------
getUniformsAndFrag
  :: forall (ctx :: GLContext). IsGLContext ctx 
  => IxShader ctx '[] (RMT :++ '[Out Xvec4 (GLFragName ctx)]) (Xvec2, Xvec4)
getUniformsAndFrag = do
  uResolution <- uniform_
  _           <- uniform_
  _           <- uniform_
  frag        <- gl_FragColor
  return (uResolution, frag)

plotFunc
  :: forall (ctx :: GLContext) i. IsGLContext ctx
  => IxFunction ctx i Xfloat "plot" (Xvec2, Xfloat) 
plotFunc = func @"plot" (Xvec2 "st", Xfloat "pct") $ \(st, pct) -> do
  t <- defineAs "thickness" 0.02
  let a = smoothstep (pct - t) pct       $ y st
      b = smoothstep pct       (pct + t) $ y st
  returnValue $ (a - b :: Xfloat)

getColor
  :: forall (ctx :: GLContext) i.
     Xvec2
  -> ((Xvec2, Xfloat) -> Xfloat)
  -> (Xfloat -> Xfloat)
  -> IxShader ctx i i Xvec4
getColor uResolution plot g = do
  st    <- defineAs "st" $ xy gl_FragCoord / uResolution
  py    <- defineAs "py" $ g $ x st
  color <- defineAs "color" $ mkvec3 py py py
  pct   <- defineAs "pct" $ plot (st, py)

  -- The background is a black to white horizontal gradient and the foreground
  -- is a green line on y = x
  bg <- defineAs "bg" $ (1.0 - pct) .* (color .: 1.0)
  let green = mkvec4 0.0 1.0 0.0 1.0
  fg <- defineAs "fg" $ pct .* green
  return $ fg + bg

type RMTPlotFrag ctx = 
  IxShader ctx '[] (RMT :++ '[ Out Xvec4 (GLFragName ctx)
                             , Function Xfloat "plot" (Xvec2, Xfloat)
                             , Main])

frag050 :: forall (ctx :: GLContext). IsGLContext ctx => RMTPlotFrag ctx ()
frag050 = do
  (uResolution, frag) <- getUniformsAndFrag 
  plot                <- plotFunc
  main_ $ do
    color <- getColor uResolution plot id
    frag .= color

frag051 = do
  (uResolution, frag) <- getUniformsAndFrag @'OpenGLContext
  plot                <- plotFunc
  main_ $ do
    color <- getColor uResolution plot (** 5.0)
    frag .= color

frag052 = do
  (uResolution, frag) <- getUniformsAndFrag @'OpenGLContext
  plot <- plotFunc
  main_ $ do
    st <- defineAs "st" $ xy gl_FragCoord / uResolution
    py <- defineAs "py" $ step 0.5 (x st)
    color <- defineAs "color" $ mkvec3 py py py
    pct   <- defineAs "pct" $ plot (st, py)
    color .= (1.0 - pct) .* color + pct .* mkvec3 0.0 1.0 0.0
    frag  .= mkvec4 (x color) (y color) (z color) 1.0

frag053 = do
  (uResolution, frag) <- getUniformsAndFrag @'OpenGLContext
  plot <- plotFunc
  main_ $ do
    st    <- defineAs "st" $ xy gl_FragCoord / uResolution
    py    <- defineAs "py" $ smoothstep 0.1 0.9 (x st)
    color <- defineAs "color" $ mkvec3 py py py
    pct   <- defineAs "pct" $ plot (st, py)
    color .= (1.0 - pct) .* color + pct .* mkvec3 0.0 1.0 0.0
    frag  .= mkvec4 (x color) (y color) (z color) 1.0

silexars1k = do
  frag        <- gl_FragColor @'OpenGLContext
  uResolution <- uniform_ @Xvec2 @"u_resolution"
  uTime       <- uniform_ @Xfloat @"u_time"
  main_ $ do
    c  <- defineAs "c" $ mkvec3 0.0 0.0 0.0
    zt <- defineAs  "z" uTime
    l  <- defineAs  "l" 0.0
    for ("i", 0) ((< 3) &&& (+= 1)) $ \i -> do
      p  <- defineAs "p" $ xy gl_FragCoord / uResolution
      uv <- defineAs "uv" p
      p   .= p - (0.5 .: 0.5)
      x p .= x uResolution / y uResolution
      zt  .= zt + 0.07
      l   .= (call "length" p :: Xfloat)
      thingy <- defineAs "thingy" $ (sin zt + 1.0) * (abs $ sin $ 1.0 * 9.0 - zt * 2.0)
      uv .= uv + (p .* (1.0 / l)) .* thingy 
      let set comp =
            comp c .= 0.01 / (call "length" $ abs $ (call2 "mod" uv 1.0) - 0.5)
      if i == 0
      then set x
      else if i == 1
           then set y
           else set z
    fxyz <- defineAs "fragXYZ" $ c .* (1.0 / l)
    frag .= fxyz .: zt
