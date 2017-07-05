{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans            #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}
module Graphics.Gristle
  ( module Graphics.Gristle
  , KnownSymbol
  ) where

import           Data.List                   (intercalate)
import           Data.Promotion.Prelude
import           Data.Promotion.Prelude.List ((:++))
import           Data.Promotion.TH
import           Data.Ratio                  (denominator, numerator)
import           Data.Singletons.TypeLits
import           Prelude                     hiding (Read, return, (>>), (>>=))

import           Graphics.Gristle.Function
import           Graphics.Gristle.IxShader
import           Graphics.Gristle.Socket


data Uniform typ name = Uniform
data In typ name = In
data Out typ name = Out

class Binding a t where
  getVertexBinding  :: t
  getUniformBinding :: t

instance KnownSymbol b => Binding (Uniform a b) (Maybe String) where
  getVertexBinding = Nothing
  getUniformBinding = Just $ symbolVal $ Proxy @b

instance KnownSymbol b => Binding (In a b) (Maybe String) where
  getVertexBinding = Just $ symbolVal $ Proxy @b
  getUniformBinding = Nothing

instance KnownSymbol b => Binding (Out a b) (Maybe String) where
  getVertexBinding = Nothing
  getUniformBinding = Nothing

instance Binding '[] [t] where
  getVertexBinding = []
  getUniformBinding = []

instance (Binding a t, Binding as [t]) => Binding (a ': as) [t] where
  getVertexBinding  = getVertexBinding  @a : getVertexBinding  @as
  getUniformBinding = getUniformBinding @a : getUniformBinding @as

class CTypeSymbol a where
  typeSymbolVal :: String

instance KnownSymbol t => CTypeSymbol (t :: Symbol) where
  typeSymbolVal = symbolVal $ Proxy @t

instance KnownSymbol t => CTypeSymbol (Socket t r w) where
  typeSymbolVal = symbolVal $ Proxy @t



-- | Some glsl evaluation contexts. This is used to choose alternate syntax in
-- cases where shader code differs between contexts, for example the @in@ keyword
-- is not available on glsl bound for a webgl context, and should be replaced
-- with @attribute@.
data GLContext = OpenGLContext | WebGLContext

-- | An easy way to get the term level value of a type of kind 'GLContext'.
class HasContext (a :: GLContext) where
  getCtx :: GLContext
instance HasContext 'OpenGLContext where
  getCtx = OpenGLContext
instance HasContext 'WebGLContext where
  getCtx = WebGLContext

typeAndName
  :: forall typ name. (KnownSymbol typ, KnownSymbol name) => (String, String)
typeAndName = (symbolVal $ Proxy @typ, symbolVal $ Proxy @name)

uniform_
  :: forall typ name ts ctx. (KnownSymbol typ, KnownSymbol name)
  => IxShader ctx ts (ts :++ '[Uniform typ name]) (Read typ)
uniform_ = acc decls (Uniform @typ @name) $ Socket nameVal
  where
    (typeVal, nameVal) = typeAndName @typ @name
    decls = unwords ["uniform", typeVal, nameVal, ";"]

in_
  :: forall typ name ts ctx. (HasContext ctx, KnownSymbol typ, KnownSymbol name)
  => IxShader ctx ts (ts :++ '[In typ name]) (Read typ)
in_ = acc decls (In @typ @name) $ Socket nameVal
  where
    (typeVal, nameVal) = typeAndName @typ @name
    dec   = case getCtx @ctx of
      OpenGLContext -> "in"
      WebGLContext  -> "attribute"
    decls = unwords [dec, typeVal, nameVal, ";"]

out_
  :: forall typ name ts ctx. (HasContext ctx, KnownSymbol typ, KnownSymbol name)
  => IxShader ctx ts (ts :++ '[Out typ name]) (Write typ)
out_ = acc decls (Out @typ @name) $ Socket nameVal
  where
    (typeVal, nameVal) = typeAndName @typ @name
    dec   = case getCtx @ctx of
      OpenGLContext -> "out"
      WebGLContext  -> "varying"
    decls = unwords [dec, typeVal, nameVal, ";"]

gl_Position
  :: forall ts ctx.
  IxShader ctx ts (ts :++ '[Out "vec4" "gl_Position"]) (Write "vec4")
gl_Position = acc [] (Out @"vec4" @"gl_Position") $ Socket "gl_Position"

type family GLFragName (a :: GLContext) where
  GLFragName 'OpenGLContext = "fragColor"
  GLFragName 'WebGLContext  = "gl_FragColor"

gl_FragColor
  :: forall ts ctx. (HasContext ctx, KnownSymbol (GLFragName ctx))
  => IxShader ctx ts (ts :++ '[Out "vec4" (GLFragName ctx)]) (Write "vec4")
gl_FragColor = acc decls (Out @"vec4" @(GLFragName ctx)) $ Socket nameVal
  where nameVal = symbolVal $ Proxy @(GLFragName ctx)
        decls  = case getCtx @ctx of
          OpenGLContext -> unwords ["out", "vec4", nameVal, ";"]
          _             -> []


infixr 1 .=
(.=)
  :: forall typ w r i ctx.
     Socket typ r 'True
  -> Socket typ 'True w
  -> IxShader ctx i i ()
(.=) (Socket a) (Socket b) = nxt_ $ unwords [a, "=", b, ";"]

$(promoteOnly [d|
  toColRow :: Symbol -> (Nat, Nat)
  toColRow "float"  = (0, 0)
  toColRow "int"    = (0, 0)
  toColRow "uint"   = (0, 0)
  toColRow "vec2"   = (2, 1)
  toColRow "vec3"   = (3, 1)
  toColRow "vec4"   = (4, 1)
  toColRow "bvec2"  = (2, 1)
  toColRow "bvec3"  = (3, 1)
  toColRow "bvec4"  = (4, 1)
  toColRow "ivec2"  = (2, 1)
  toColRow "ivec3"  = (3, 1)
  toColRow "ivec4"  = (4, 1)
  toColRow "uvec2"  = (2, 1)
  toColRow "uvec3"  = (3, 1)
  toColRow "uvec4"  = (4, 1)
  toColRow "mat2"   = (2, 2)
  toColRow "mat2x2" = (2, 2)
  toColRow "mat2x3" = (2, 3)
  toColRow "mat2x4" = (2, 4)
  toColRow "mat3"   = (3, 3)
  toColRow "mat3x2" = (2, 2)
  toColRow "mat3x3" = (3, 3)
  toColRow "mat3x4" = (3, 4)
  toColRow "mat4"   = (4, 4)
  toColRow "mat4x2" = (4, 2)
  toColRow "mat4x3" = (4, 3)
  toColRow "mat4x4" = (4, 4)

  showInt :: Nat -> Symbol
  showInt 0 = "0"
  showInt 1 = "1"
  showInt 2 = "2"
  showInt 3 = "3"
  showInt 4 = "4"
  showInt 5 = error "showInt only goes to 5, lol"

  matFromColRow :: (Nat, Nat) -> Symbol
  matFromColRow (2, 2) = "mat2"
  matFromColRow (2, 3) = "mat2x3"
  matFromColRow (2, 4) = "mat2x4"
  matFromColRow (3, 3) = "mat3"
  matFromColRow (3, 2) = "mat3x2"
  matFromColRow (3, 4) = "mat3x4"
  matFromColRow (4, 4) = "mat4"
  matFromColRow (4, 2) = "mat4x2"
  matFromColRow (4, 3) = "mat4x3"
  matFromColRow _      = error "unsupported matrix column or row"

  multiply :: Symbol -> Symbol -> Symbol
  multiply a b
    | a == b = a
    | otherwise = b
    | (0,0) <- toColRow a = b
    | (0,0) <- toColRow b = a
    | (_,1) <- toColRow a
    , (_,1) <- toColRow b
    , True  <- a /= b =
        error $ concat ["no operation '*' exists for ", a, " and ", b]
    | otherwise = case (toColRow a, toColRow b) of
      ((ca, ra), (cb, rb)) ->
        case ra == 1 || rb == 1 of
        True -> case ca == cb of
            True -> a
            False -> error $ concat ["no operation '*' exists for ", a, " and ", b]
        False -> matFromColRow (ca, rb)
 |])

infixl 7 .*
(.*)
  :: forall w3 typ1 typ2 w1 w2.
     Socket typ1 'True w1
  -> Socket typ2 'True w2
  -> Socket (Multiply typ1 typ2) 'True w3
(.*) (Socket a) (Socket b) = Socket $ unwords [a, "*", b]

class IsR1 (a :: Symbol) where
  type ToR1 a :: Symbol
instance IsR1 "vec2" where
  type ToR1 "vec2" = "float"
instance IsR1 "vec3" where
  type ToR1 "vec3" = "float"
instance IsR1 "vec4" where
  type ToR1 "vec4" = "float"
instance IsR1 "ivec2" where
  type ToR1 "ivec2" = "int"
instance IsR1 "ivec3" where
  type ToR1 "ivec3" = "int"
instance IsR1 "ivec4" where
  type ToR1 "ivec4" = "int"
instance IsR1 "bvec2" where
  type ToR1 "bvec2" = "bool"
instance IsR1 "bvec3" where
  type ToR1 "bvec3" = "bool"
instance IsR1 "bvec4" where
  type ToR1 "bvec4" = "bool"
instance IsR1 "uvec2" where
  type ToR1 "uvec2" = "uint"
instance IsR1 "uvec3" where
  type ToR1 "uvec3" = "uint"
instance IsR1 "uvec4" where
  type ToR1 "uvec4" = "uint"

x
  :: forall (t :: Symbol) (w :: Bool). IsR1 t
  => Socket t 'True w -> Socket (ToR1 t) 'True w
x (Socket s) = Socket $ concat ["(", s, ").x"]

class IsR2 (a :: Symbol) where
  type ToR2 a :: Symbol
instance IsR2 "vec2" where
  type ToR2 "vec2" = "float"
instance IsR2 "vec3" where
  type ToR2 "vec3" = "float"
instance IsR2 "vec4" where
  type ToR2 "vec4" = "float"
instance IsR2 "ivec2" where
  type ToR2 "ivec2" = "int"
instance IsR2 "ivec3" where
  type ToR2 "ivec3" = "int"
instance IsR2 "ivec4" where
  type ToR2 "ivec4" = "int"
instance IsR2 "bvec2" where
  type ToR2 "bvec2" = "bool"
instance IsR2 "bvec3" where
  type ToR2 "bvec3" = "bool"
instance IsR2 "bvec4" where
  type ToR2 "bvec4" = "bool"
instance IsR2 "uvec2" where
  type ToR2 "uvec2" = "uint"
instance IsR2 "uvec3" where
  type ToR2 "uvec3" = "uint"
instance IsR2 "uvec4" where
  type ToR2 "uvec4" = "uint"

y
  :: forall (t :: Symbol) (w :: Bool). IsR2 t
  => Socket t 'True w -> Socket (ToR2 t) 'True w
y (Socket s) = Socket $ concat ["(", s, ").y"]

gl_FragCoord :: Read "vec4"
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

f :: Float -> Read "float"
f = Socket . show

dbl :: Double -> Read "float"
dbl = Socket . show

mkvec4
  :: Socket "float" 'True w1
  -> Socket "float" 'True w2
  -> Socket "float" 'True w3
  -> Socket "float" 'True w4
  -> Socket "vec4" 'True 'False
mkvec4 (Socket a) (Socket b) (Socket c) (Socket d) =
  Socket $ "vec4(" ++ intercalate "," [a, b, c, d] ++ ")"

class IsNumValue typ
instance IsNumValue "float"
instance IsNumValue "int"
instance IsNumValue "uint"

instance Num (Socket (t :: Symbol) 'True (w :: Bool)) where
  (+) = callInfix "+"
  (-) = callInfix "-"
  (*) = callInfix "*"
  negate (Socket a) = Socket $ concat ["(-", a, ")"]
  abs    = call "abs"
  signum = call "sign"
  fromInteger = Socket . show . (fromInteger :: Integer -> Float)

instance Fractional (Socket (t :: Symbol) 'True (w :: Bool)) where
  fromRational a = Socket $ show $
    (fromIntegral (numerator a) :: Float) / fromIntegral (denominator a)
  (/) = callInfix "/"

instance Floating (Socket (t :: Symbol) 'True (w :: Bool)) where
  pi = Socket $ show (pi :: Float)
  exp  = call "exp"
  log  = call "log"
  sqrt = call "sqrt"
  (**) = call2 "pow"
  logBase a b = log b / log a
  sin = call "sin"
  cos = call "cos"
  tan = call "tan"
  asin = call "asin"
  acos = call "acos"
  atan = call "atan"
  sinh = call "sinh"
  cosh = call "cosh"
  tanh = call "tanh"
  asinh = call "asinh"
  acosh = call "acosh"
  atanh = call "atanh"

smoothstep
  :: forall (t :: Symbol) (w1 :: Bool) (w2 :: Bool) (w3 :: Bool).
     Socket t 'True w1
  -> Socket t 'True w2
  -> Socket t 'True w3
  -> Read t
smoothstep = call3 "smoothstep"


vertex :: IxShader ctx '[] '[ Uniform "vec2" "u_resolution"
                            , Out "vec4" "gl_Position"
                            ] ()
vertex = do
  res <- uniform_ @"vec2" @"u_resolution"
  pos <- gl_Position
  let ps@(a, b) = (int "a", int "b")
  myFunc <- func @"int" @"myFunc" ps $ funcReturnVal $ a + b

  main_ $ do
    pa <- intM "paramA"
    pb <- intM "paramB"
    c  <- intM "c"
    pa .= toInt 0
    pb .= toInt 1
    c  .= myFunc (pa, pb)
    return ()
