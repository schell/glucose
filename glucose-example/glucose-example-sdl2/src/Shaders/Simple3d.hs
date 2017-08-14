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
module Shaders.Simple3d where

import Graphics.Gristle

type IxVert (ctx :: GLContext) (ts :: [*]) a = IsGLContext ctx => IxShader ctx '[] ts a
type IxFrag ctx ts a = IxVert ctx ts a

diffuseLightIntesity
  :: (Readable a b, Readable c d, ReadFrom a ~ ReadFrom c, ReadFrom a ~ Xvec3)
  => a 
  -- ^ The light source intensity
  -> b 
  -- ^ The reflection coefficient
  -> c 
  -- ^ The direction of the surface point to the light source
  -> d 
  -- ^ The normal vector at the surface point
  -> Xvec3
diffuseLightIntesity ld kd s n = ld * kd .* max (dot s n) 0.0
   

diffuseVertex
  :: IxVert ctx '[ In      Xvec3 "VertexPosition"
                 , In      Xvec3 "VertexNormal"
                 --, In Xvec4 "color"
                 , Out     Xvec4 "gl_Position"
                 , Out     Xvec3 "LightIntensity"
                 , Uniform Xvec4 "LightPosition" -- position in eye coords
                 , Uniform Xvec3 "Kd"            -- diffuse reflectivity
                 , Uniform Xvec3 "Ld"            -- light source intensity 
                 , Uniform Xmat4 "ModelViewMatrix"
                 , Uniform Xmat3 "NormalMatrix"
                 , Uniform Xmat4 "ProjectionMatrix"
                 , Main
                 ] () 
diffuseVertex = do
  vertexPosition <- in_
  vertexNormal   <- in_

  glPosition     <- gl_Position
  lightIntensity <- out_

  lightPosition    <- uniform_
  kd               <- uniform_
  ld               <- uniform_
  modelViewMatrix  <- uniform_
  normalMatrix     <- uniform_
  projectionMatrix <- uniform_

  main_ $ do
    -- Convert normal and position to eye coords
    tnorm   <- def "tnorm" $
      normalize $ normalMatrix .* vertexNormal
    eyeCoords <- def "eyeCoords" $
      modelViewMatrix .* (vertexPosition .: 1.0)
    s <- def "s" $
      normalize $ xyz $ lightPosition - eyeCoords 
    -- The diffuse shading equation
    lightIntensity .= diffuseLightIntesity ld kd s tnorm 
    -- Convert position to clip coordinates and pass along
    glPosition .= projectionMatrix .* modelViewMatrix .* (vertexPosition .: 1.0)

diffuseFragment
  :: IxFrag ctx '[ In Xvec3 "LightIntensity"
                 , Out Xvec4 (GLFragName ctx)
                 , Main
                 ] ()
diffuseFragment = do
  lightIntensity <- in_
  color <- gl_FragColor
  main_ $ color .= lightIntensity .: 1.0 
