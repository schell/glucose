{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans            #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}
module Graphics.Gristle where

import           Control.Arrow                  ((&&&))
import           Control.Monad.Indexed
import           Data.Either                    (lefts, rights)
import           Data.Promotion.Prelude.List    ((:++))
import           Data.Singletons.TH
import           Data.Singletons.TypeLits
import           Language.GLSL
import           Prelude                        hiding (return, (>>), (>>=))
import           Text.Parsec.Error              (Message (..), errorMessages,
                                                 messageString)
import           Text.PrettyPrint.HughesPJClass

toSrc :: Pretty a => a -> String
toSrc = show . pPrint

--data Vec2 = Vec2
--data Vec4 = Vec4
--data Mat4 = Mat4
--
--instance ToShaderDecl Vec2 where
--  shaderDecl = ["vec2"]
--
--instance ToShaderDecl Vec4 where
--  shaderDecl = ["vec4"]
--
--instance ToShaderDecl Mat4 where
--  shaderDecl = ["mat4"]
--
data Uniform_ typ name = Uniform_
data In_ typ name = In_
data Out_ typ name = Out_
--
--class ToShaderDecl a where
--  shaderDecl :: [String]
--
--instance KnownSymbol a => ToShaderDecl a where
--  shaderDecl = [symbolVal (Proxy @a)]
--
--instance KnownSymbol a => ToShaderDecl (Proxy a) where
--  shaderDecl = [symbolVal (Proxy @a)]
--
--instance ToShaderDecl () where
--  shaderDecl = []
--
--globalDecl :: String -> String -> String -> [String]
--globalDecl k t n = [unwords [k, t, n ++ ";\n"]]
--
--instance (ToShaderDecl typ, ToShaderDecl name) => ToShaderDecl (Uniform_ typ name) where
--  shaderDecl = globalDecl "uniform" (concat $ shaderDecl @typ) (concat $ shaderDecl @name)
--
--instance (ToShaderDecl typ, ToShaderDecl name) => ToShaderDecl (In_ typ name) where
--  shaderDecl = globalDecl "in" (concat $ shaderDecl @typ) (concat $ shaderDecl @name)
--
--instance (ToShaderDecl typ, ToShaderDecl name) => ToShaderDecl (Out_ typ name) where
--  shaderDecl = globalDecl "out" (concat $ shaderDecl @typ) (concat $ shaderDecl @name)
--
data Function typ name params body = Function
--
--instance (ToShaderDecl typ, ToShaderDecl name, ToShaderDecl params, ToShaderDecl body)
--  => ToShaderDecl (Function typ name params body) where
--  shaderDecl = [ unwords $ map concat [ shaderDecl @typ
--                                      , shaderDecl @name
--                                      , shaderDecl @params
--                                      , ["{"]
--                                      ]
--               , concat (shaderDecl @body) ++ ""
--               , "}"
--               ]
--
--instance ToShaderDecl '[] where
--  shaderDecl = []
--
--instance (ToShaderDecl t, ToShaderDecl ts) => ToShaderDecl (t ': ts) where
--  shaderDecl = [unwords $ map concat [shaderDecl @t, shaderDecl @ts]]
--
data Shader i j n where
  ShNxt :: [Either [Message] [ExternalDeclaration]] -> n -> Shader i j n
  ShAdd :: [Either [Message] [ExternalDeclaration]] -> t -> n -> Shader i (i :++ '[t]) n

--instance ToShaderDecl j => ToShaderDecl (Shader i j n) where
--  shaderDecl = [unlines $ shaderDecl @j]
--
unN :: Shader i j n -> n
unN = \case
  (ShNxt _ n) -> n
  (ShAdd _ _ n) -> n

unDecl :: Shader i j n -> [Either [Message] [ExternalDeclaration]]
unDecl (ShNxt d _)   = d
unDecl (ShAdd d _ _) = d

instance IxFunctor Shader where
  imap f sh = ShNxt (unDecl sh) $ f (unN sh)

instance IxPointed Shader where
  ireturn = ShNxt []

instance IxApplicative Shader where
  iap mf mx = ShNxt (unDecl mf ++ unDecl mx) $ unN mf $ unN mx

instance IxMonad Shader where
  ibind amb ma =
    let (dsa, a) = unDecl &&& unN $ ma
        (dsb, b) = unDecl &&& unN $ amb a
    in ShNxt (dsa ++ dsb) b

data SocketMode = SocketRead
                | SocketWrite
                | SocketReadWrite

-- | A socket is simply a C-like "variable". It's a spot where you can read,
-- write or read and write to/from.
newtype Socket typ mode = Socket String

declarations :: String -> [Either [Message] [ExternalDeclaration]]
declarations str = case parse str of
  Left err                   -> [Left $ errorMessages err]
  Right (TranslationUnit ds) -> [Right ds]

typeAndName
  :: forall typ name. (KnownSymbol typ, KnownSymbol name) => (String, String)
typeAndName = (symbolVal $ Proxy @typ, symbolVal $ Proxy @name)

-- | Does three things - adds a uniform input into the global type of the shader,
-- encodes one or more @ExternalDeclaration@s and returns a socket/slot of the
-- uniform.
uniform_
  :: forall typ name ts. (KnownSymbol typ, KnownSymbol name)
  => Shader ts (ts :++ '[Uniform_ typ name]) (Socket typ 'SocketRead)
uniform_ = ShAdd decls (Uniform_ @typ @name) $ Socket nameVal
  where
    (typeVal, nameVal) = typeAndName @typ @name
    decls = declarations $ unwords ["uniform", typeVal, nameVal, ";"]

in_
  :: forall typ name ts. (KnownSymbol typ, KnownSymbol name)
  => Shader ts (ts :++ '[In_ typ name]) (Socket typ 'SocketRead)
in_ = ShAdd decls (In_ @typ @name) $ Socket nameVal
  where
    (typeVal, nameVal) = typeAndName @typ @name
    decls = declarations $ unwords ["in", typeVal, nameVal, ";"]

out_
  :: forall typ name ts. (KnownSymbol typ, KnownSymbol name)
  => Shader ts (ts :++ '[Out_ typ name]) (Socket typ 'SocketWrite)
out_ = ShAdd decls (Out_ @typ @name) $ Socket nameVal
  where
    (typeVal, nameVal) = typeAndName @typ @name
    decls = declarations $ unwords ["out", typeVal, nameVal, ";"]

gl_Position
  :: forall ts.
  Shader ts (ts :++ '[Out_ "vec4" "gl_Position"]) (Socket "vec4" 'SocketWrite)
gl_Position = ShAdd [] (Out_ @"vec4" @"gl_Position") $ Socket "gl_Position"

--function_
--  :: forall typ name params body ts.
--  typ -> name -> params -> body ->
--  Shader ts (ts :++ '[Function typ name params body]) (Function typ name params body)
--function_ _ _ _ _ = ShAdd Function

type Main body = Function "void" "main" () body

--main_ :: () -> body -> Shader ts (ts :++ '[Main body]) (Main body)
--main_ () _ = ShAdd Function

(>>=) :: forall i j k a b. Shader i j a -> (a -> Shader j k b) -> Shader i k b
a >>= b = a >>>= b

return :: forall a i. a -> Shader i i a
return = ireturn

(>>) :: forall i j a k b. Shader i j a -> Shader j k b -> Shader i k b
a >> b = a >>>= const b

void :: Shader i k a -> Shader i k ()
void ma = ma >> return ()

type VertexShader = Shader '[]

vertex :: Shader '[] '[ Out_     "vec4" "gl_Position"
                      , In_      "vec2" "position"
                      , In_      "vec4" "color"
                      , Out_     "vec4" "fcolor"
                      , Uniform_ "mat4" "projection"
                      , Uniform_ "mat4" "modelview"
                      ] ()
vertex = do
  glPos  <- gl_Position

  pos    <- in_ @"vec2" @"position"
  color  <- in_ @"vec4" @"color"

  fcolor <- out_ @"vec4" @"fcolor"

  proj   <- uniform_ @"mat4" @"projection"
  modl   <- uniform_ @"mat4" @"modelview"
  return ()

--  void $ main_ () $ Proxy @"functionbody;\n"
--
shaderSource
  :: Shader '[] j a
  -> Either String String
shaderSource sh = case (lefts $ unDecl sh, rights $ unDecl sh) of
  ( [], decls) -> Right $ toSrc $ TranslationUnit $ concat decls
  (ers,     _) -> Left $ unlines $ map messageString $ concat ers
--
--testSrc :: String
--testSrc = shaderSource $ start >> vertex
--  where start :: Shader '[] '[] ()
--        start = return ()
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
