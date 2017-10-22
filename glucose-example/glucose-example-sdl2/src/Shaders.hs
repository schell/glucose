{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}
module Shaders where

import           Graphics.IxShader

------------------------------------------------------------------------------
-- Stuff goes to mainline gristle
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Book of Shaders
------------------------------------------------------------------------------
passthruVertex
  :: forall (ctx :: GLContext). IsGLContext ctx
  => IxVertex ctx '[] '[ In Xvec2 "position"
                       , Out Xvec4 "gl_Position"
                       , Main
                       ] ()
passthruVertex = do
  p     <- in_
  glpos <- gl_Position
  main_ $ glpos .= mkvec4 (x p) (y p) 0.0 1.0

helloFrag
  :: forall (ctx :: GLContext). IsGLContext ctx
  => IxFragment ctx '[] '[ Out Xvec4 (GLFragName ctx)
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
  => IxFragment ctx '[] '[ Uniform Xfloat "u_time"
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
  IxFragment ctx '[] (RMT :++ '[Out Xvec4 (GLFragName ctx), Main])


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
  => IxFragment ctx '[] (RMT :++ '[Out Xvec4 (GLFragName ctx)])
       (Uniform Xvec2 "u_resolution", Out Xvec4 (GLFragName ctx))
getUniformsAndFrag = do
  uResolution <- uniform_
  _           <- uniform_
  _           <- uniform_
  frag        <- gl_FragColor
  return (uResolution, frag)

plotFunc
  :: forall (ctx :: GLContext) i. IsGLContext ctx
  => IxFragment ctx i (i :++ '[Function Xfloat "plot" (Xvec2, Xfloat)]) ((Xvec2, Xfloat) -> Xfloat)
plotFunc = func @"plot" (Xvec2 "st", Xfloat "pct") $ \(st, pct) -> do
  t <- def "thickness" 0.02
  let a = smoothstep (pct - t) pct       $ y st
      b = smoothstep pct       (pct + t) $ y st
  returnValue $ a - b

getColor
  :: forall (ctx :: GLContext) i.
     Xvec2
  -> ((Xvec2, Xfloat) -> Xfloat)
  -> (Xfloat -> Xfloat)
  -> IxFragment ctx i i Xvec4
getColor uResolution plot g = do
  st    <- def "st" $ xy gl_FragCoord / uResolution
  py    <- def "py" $ g $ x st
  color <- def "color" $ mkvec3 py py py
  pct   <- def "pct" $ plot (st, py)

  -- The background is a black to white horizontal gradient and the foreground
  -- is a green line on y = x
  bg <- def "bg" $ (1.0 - pct) .* (color .: 1.0)
  let green = mkvec4 0.0 1.0 0.0 1.0
  fg <- def "fg" $ pct .* green
  return $ fg + bg

type RMTPlotFrag ctx =
  IxFragment ctx '[] (RMT :++ '[ Out Xvec4 (GLFragName ctx)
                               , Function Xfloat "plot" (Xvec2, Xfloat)
                               , Main])

frag050 :: forall (ctx :: GLContext). IsGLContext ctx => RMTPlotFrag ctx ()
frag050 = do
  (uResolution, frag) <- getUniformsAndFrag
  plot                <- plotFunc
  main_ $ do
    color <- getColor (cast uResolution) plot id
    frag .= color

frag051 = do
  (uResolution, frag) <- getUniformsAndFrag @'OpenGLContext
  plot                <- plotFunc
  main_ $ do
    color <- getColor (cast uResolution) plot (** 5.0)
    frag .= color

frag052 = do
  (uResolution, frag) <- getUniformsAndFrag @'OpenGLContext
  plot <- plotFunc
  main_ $ do
    st <- def "st" $ xy gl_FragCoord / uResolution
    py <- def "py" $ step 0.5 (x st)
    color <- def "color" $ mkvec3 py py py
    pct   <- def "pct" $ plot (st, py)
    color .= (1.0 - pct) .* color + pct .* mkvec3 0.0 1.0 0.0
    frag  .= mkvec4 (x color) (y color) (z color) 1.0

frag053 = do
  (uResolution, frag) <- getUniformsAndFrag @'OpenGLContext
  plot <- plotFunc
  main_ $ do
    st    <- def "st" $ xy gl_FragCoord / uResolution
    py    <- def "py" $ smoothstep 0.1 0.9 (x st)
    color <- def "color" $ mkvec3 py py py
    pct   <- def "pct" $ plot (st, py)
    color .= (1.0 - pct) .* color + pct .* mkvec3 0.0 1.0 0.0
    frag  .= mkvec4 (x color) (y color) (z color) 1.0

silexars1k
  :: forall (ctx :: GLContext). IsGLContext ctx
  => IxFragment ctx '[] '[ Out Xvec4 (GLFragName ctx)
                         , Uniform Xvec2 "u_resolution"
                         , Uniform Xfloat "u_time"
                         --, Function Xvoid "mainImage" (In Xvec4 "fragColor", In Xvec4 "fragCoord")
                         , Main
                         ] ()
silexars1k = do
  fragColor   <- gl_FragColor
  r           <- uniform_
  t           <- uniform_
  main_ $ do
    c <- define $ Xvec3 "c"
    l <- define $ Xfloat "l"
    z <- def "z" $ cast t
    for ("i", 0) ((< 3) &&& (+= 1)) $ \i -> do
      uv <- define $ Xvec2 "uv"
      p <- def "p" $ xy gl_FragCoord / r
      uv  .= p
      p   .= p - mkvec2 0.5 0.5
      x p .= x p * (x r / y r)
      use $ z += 0.07
      l   .= length p
      mlt <- def "mult" $ (sin z + 1.0) * abs (sin $ l * 9.0 - z * 2.0)
      uv  .= uv + ((p .* (1.0 / l)) .* mlt)
      c `at` i .= 0.01 / length (abs (mod uv 1.0) - (0.5 .: 0.5))
    fragColor .= (c .* (1.0 / l)) .: t


printSilexars1k = putStrLn $ onlySrc $ silexars1k @'OpenGLContext
