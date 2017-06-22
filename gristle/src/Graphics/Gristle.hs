{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures      #-}
module Graphics.Gristle where

import           Control.Monad.Indexed
import           Data.Promotion.Prelude.List ((:++))
import           Data.Proxy
import           Data.Singletons.TypeLits
import           Prelude                     hiding (return, (>>), (>>=))


data Vec2 = Vec2
data Vec4 = Vec4
data Mat4 = Mat4

instance ToShaderDecl Vec2 where
  shaderDecl = "vec2"

instance ToShaderDecl Vec4 where
  shaderDecl = "vec4"

instance ToShaderDecl Mat4 where
  shaderDecl = "mat4"

data Uniform typ name = Uniform
data In typ name = In
data Out typ name = Out

class ToShaderDecl a where
  shaderDecl :: [String]

instance KnownSymbol a => ToShaderDecl a where
  shaderDecl = [symbolVal (Proxy @a)]

instance KnownSymbol a => ToShaderDecl (Proxy a) where
  shaderDecl = [symbolVal (Proxy @a)]

instance ToShaderDecl () where
  shaderDecl = []

globalDecl :: String -> String -> String -> String
globalDecl k t n = [k, t, n ++ ";\n"]

instance (ToShaderDecl typ, ToShaderDecl name) => ToShaderDecl (Uniform typ name) where
  shaderDecl = globalDecl "uniform" (shaderDecl @typ) (shaderDecl @name)

instance (ToShaderDecl typ, ToShaderDecl name) => ToShaderDecl (In typ name) where
  shaderDecl = globalDecl "in" (shaderDecl @typ) (shaderDecl @name)

instance (ToShaderDecl typ, ToShaderDecl name) => ToShaderDecl (Out typ name) where
  shaderDecl = globalDecl "out" (shaderDecl @typ) (shaderDecl @name)

data Function typ name params body = Function

instance (ToShaderDecl typ, ToShaderDecl name, ToShaderDecl params, ToShaderDecl body)
  => ToShaderDecl (Function typ name params body) where
  shaderDecl = [ unwords [ shaderDecl @typ
                         , shaderDecl @name
                         , shaderDecl @params ++ "{\n"
                         ]
               , shaderDecl @body ++ "\n"
               , "}\n\n"
               ]

instance ToShaderDecl '[] where
  shaderDecl = []

instance (ToShaderDecl t, ToShaderDecl ts) => ToShaderDecl (t ': ts) where
  shaderDecl = unwords [shaderDecl @t, shaderDecl @ts]

data Shader i j n where
  ShNxt :: n -> Shader i j n
  ShAdd :: n -> Shader i (i :++ '[n]) n

instance (ToShaderDecl i, ToShaderDecl j) => ToShaderDecl (Shader i j n) where
  shaderDecl = [shaderDecl @i, shaderDecl @j]

unN :: Shader i j n -> n
unN = \case
  (ShNxt n) -> n
  (ShAdd n) -> n

instance IxFunctor Shader where
  imap f sh = ShNxt $ f $ unN sh

instance IxPointed Shader where
  ireturn = ShNxt

instance IxApplicative Shader where
  iap mf mx = ShNxt $ unN mf $ unN mx

instance IxMonad Shader where
  ibind amb ma = ShNxt $ unN $ amb $ unN ma

uniform_
  :: forall typ name ts.
  Shader ts (ts :++ '[Uniform typ name]) (Uniform typ name)
uniform_ = ShAdd Uniform

in_
  :: forall typ name ts.
  Shader ts (ts :++ '[In typ name]) (In typ name)
in_ = ShAdd In

out_
  :: forall typ name ts.
  Shader ts (ts :++ '[Out typ name]) (Out typ name)
out_ = ShAdd Out

gl_Position
  :: forall ts.
  Shader ts (ts :++ '[Out Vec4 "gl_Position"]) (Out Vec4 "gl_Position")
gl_Position = ShAdd Out

function_
  :: forall typ name params body ts.
  typ -> name -> params -> body ->
  Shader ts (ts :++ '[Function typ name params body]) (Function typ name params body)
function_ _ _ _ _ = ShAdd Function

type Main body = Function "void" "main" () body

main_ :: () -> body -> Shader ts (ts :++ '[Main body]) (Main body)
main_ () _ = ShAdd Function

(>>=) :: forall i j k a b. Shader i j a -> (a -> Shader j k b) -> Shader i k b
a >>= b = a >>>= b

return :: forall a i. a -> Shader i i a
return = ireturn

(>>) :: forall i j a k b. Shader i j a -> Shader j k b -> Shader i k b
a >> b = a >>>= const b

void :: Shader i k a -> Shader i k ()
void ma = ma >> return ()

vertex = do
  glPos  <- gl_Position

  pos    <- in_ @Vec2 @"position"
  color  <- in_ @Vec4 @"color"

  fcolor <- out_ @Vec4 @"fcolor"

  proj   <- uniform_ @Mat4 @"projection"
  modl   <- uniform_ @Mat4 @"modelview"
  void $ main_ () $ Proxy @"functionbody;\n"

shaderSource
  :: forall i j n. (ToShaderDecl i, ToShaderDecl j)
  => Shader i j n
  -> String
shaderSource _ = shaderDecl @(Shader i j n)

testSrc :: String
testSrc = shaderSource $ start >> vertex
  where start :: Shader '[] '[] ()
        start = return ()
{-
> putStrLn testSrc
 out vec4 gl_Position;
 in vec2 position;
 in vec4 color;
 out vec4 fcolor;
 uniform mat4 projection;
 uniform mat4 modelview;
 void main {
 functionbody;

 }
-}
--------------------------------------------------------------------------------
-- Goal: write shaders in haskell and make it look as close as possible to real
-- glsl
--------------------------------------------------------------------------------
--vertex :: VertexShader '[Vec2, Vec4] '[Mat4, Mat4] '[Vec4]
--vertex = do
--  glPos  <- gl_Position
--
--  pos    <- in_  vec2 "position"
--  color  <- in_  vec4 "color"
--
--  fcolor <- out_ vec4 "fcolor"
--
--  proj   <- uniform_ mat4 "projection"
--  modl   <- uniform_ mat4 "modelview"
--  main () $ do
--    fcolor $= color
--    glPos  $= projection * modelview * mkVec4 (pos^.x) (pos^.y) 0 1
--
--fragment :: FragmentShader '[Vec4] '[] '[Vec4]
--fragment = do
--  version_ 3 3 0 core
--  fcolor <- in_ Vec4 "fcolor"
--  glFrag <- gl_FragColor
--  main () $ glFrag $= fcolor
