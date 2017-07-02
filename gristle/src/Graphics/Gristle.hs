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

import           Control.Arrow                  ((&&&))
import           Control.Monad.Indexed
import           Data.List                      (intercalate)
import           Data.Promotion.Prelude
import           Data.Promotion.Prelude.List    ((:++))
import           Data.Promotion.TH
import           Data.Ratio                     (denominator, numerator)
import           Data.Singletons.TypeLits
import           Language.GLSL                  (TranslationUnit (..), parse)
import           Prelude                        hiding (return, (>>), (>>=))
import           Text.PrettyPrint.HughesPJClass

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

data IxShader ctx i j n where
  ShNxt :: [String] -> n -> IxShader ctx i j n
  ShAcc :: [String] -> t -> n -> IxShader ctx i (i :++ '[t]) n

unN :: IxShader ctx i j n -> n
unN = \case
  (ShNxt _ n) -> n
  (ShAcc _ _ n) -> n

unDecl :: IxShader ctx i j n -> [String]
unDecl (ShNxt d _)   = d
unDecl (ShAcc d _ _) = d

instance IxFunctor (IxShader ctx) where
  imap f sh = ShNxt (unDecl sh) $ f (unN sh)

instance IxPointed (IxShader ctx) where
  ireturn = ShNxt []

instance IxApplicative (IxShader ctx) where
  iap mf mx = ShNxt (unDecl mf ++ unDecl mx) $ unN mf $ unN mx

instance IxMonad (IxShader ctx) where
  ibind amb ma =
    let (dsa, a) = unDecl &&& unN $ ma
        (dsb, b) = unDecl &&& unN $ amb a
    in ShNxt (dsa ++ dsb) b

-- | A socket is simply a C-like "variable". It's a spot where you can read,
-- write or read and write to/from.
newtype Socket typ read write = Socket String deriving (Show)
type SocketReadOnly  typ = Socket typ 'True  'False
type SocketWriteOnly typ = Socket typ 'False 'True
type SocketReadWrite typ = Socket typ 'True  'True

-- | Construct a new socket.
socket
  :: forall typ (read :: Bool) (write :: Bool).
     String
  -> Socket typ read write
socket = Socket

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

-- | Does three things - appends a type to the IxMonad @j@, encodes one or more
-- lines of shader code and returns something. This is the main entry point for
-- any shader building code, and also an easy escape hatch.
acc
  :: forall typ a i ctx. String
  -> typ
  -> a
  -> IxShader ctx i (i :++ '[typ]) a
acc dec = ShAcc (lines dec)

nxt
  :: forall i a ctx.
     String
  -> a
  -> IxShader ctx i i a
nxt dec = ShNxt (lines dec)

nxt_ :: forall i ctx. String -> IxShader ctx i i ()
nxt_ dec = nxt dec ()

sub
  :: forall i j a ctx.
     String
  -> String
  -> IxShader ctx i j a
  -> IxShader ctx i j a
sub open close sh = do
  nxt open ()
  a <- sh
  nxt close ()
  return a

sub_
  :: forall i j a ctx.
     String
  -> String
  -> IxShader ctx i j a
  -> IxShader ctx i j ()
sub_ open close sh = sub open close sh >> return ()

uniform_
  :: forall typ name ts ctx. (KnownSymbol typ, KnownSymbol name)
  => IxShader ctx ts (ts :++ '[Uniform typ name]) (SocketReadOnly typ)
uniform_ = acc decls (Uniform @typ @name) $ Socket nameVal
  where
    (typeVal, nameVal) = typeAndName @typ @name
    decls = unwords ["uniform", typeVal, nameVal, ";"]

in_
  :: forall typ name ts ctx. (HasContext ctx, KnownSymbol typ, KnownSymbol name)
  => IxShader ctx ts (ts :++ '[In typ name]) (SocketReadOnly typ)
in_ = acc decls (In @typ @name) $ Socket nameVal
  where
    (typeVal, nameVal) = typeAndName @typ @name
    dec   = case getCtx @ctx of
      OpenGLContext -> "in"
      WebGLContext  -> "attribute"
    decls = unwords [dec, typeVal, nameVal, ";"]

out_
  :: forall typ name ts ctx. (HasContext ctx, KnownSymbol typ, KnownSymbol name)
  => IxShader ctx ts (ts :++ '[Out typ name]) (SocketWriteOnly typ)
out_ = acc decls (Out @typ @name) $ Socket nameVal
  where
    (typeVal, nameVal) = typeAndName @typ @name
    dec   = case getCtx @ctx of
      OpenGLContext -> "out"
      WebGLContext  -> "varying"
    decls = unwords [dec, typeVal, nameVal, ";"]

gl_Position
  :: forall ts ctx.
  IxShader ctx ts (ts :++ '[Out "vec4" "gl_Position"]) (SocketWriteOnly "vec4")
gl_Position = acc [] (Out @"vec4" @"gl_Position") $ Socket "gl_Position"

type family GLFragName (a :: GLContext) where
  GLFragName 'OpenGLContext = "fragColor"
  GLFragName 'WebGLContext  = "gl_FragColor"

gl_FragColor
  :: forall ts ctx. (HasContext ctx, KnownSymbol (GLFragName ctx))
  => IxShader ctx ts (ts :++ '[Out "vec4" (GLFragName ctx)]) (SocketWriteOnly "vec4")
gl_FragColor = acc decls (Out @"vec4" @(GLFragName ctx)) $ Socket nameVal
  where nameVal = symbolVal $ Proxy @(GLFragName ctx)
        decls  = case getCtx @ctx of
          OpenGLContext -> unwords ["out", "vec4", nameVal, ";"]
          _             -> []

(>>=) :: forall i j k a b ctx. IxShader ctx i j a -> (a -> IxShader ctx j k b) -> IxShader ctx i k b
a >>= b = a >>>= b

return :: forall a i ctx. a -> IxShader ctx i i a
return = ireturn

(>>) :: forall i j a k b ctx. IxShader ctx i j a -> IxShader ctx j k b -> IxShader ctx i k b
a >> b = a >>>= const b

void :: IxShader ctx i k a -> IxShader ctx i k ()
void ma = ma >> return ()

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

x :: IsR1 typ => Socket typ 'True w -> Socket (ToR1 typ) 'True w
x (Socket s) = Socket $ s ++ ".x"

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

y :: IsR2 typ => Socket typ 'True w -> Socket (ToR2 typ) 'True w
y (Socket s) = Socket $ s ++ ".y"

f :: Float -> SocketReadOnly "float"
f = Socket . show

dbl :: Double -> SocketReadOnly "float"
dbl = Socket . show

int :: Int -> SocketReadOnly "int"
int = Socket . show

uint :: Word -> SocketReadOnly "uint"
uint = Socket . show

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

instance IsNumValue typ => Num (SocketReadOnly typ) where
  (+) (Socket a) (Socket b) = Socket $ unwords [a, "+", b]
  (-) (Socket a) (Socket b) = Socket $ unwords [a, "-", b]
  (*) (Socket a) (Socket b) = Socket $ unwords [a, "*", b]
  negate (Socket a) = Socket $ concat ["(-", a, ")"]
  abs (Socket a)    = Socket $ concat ["abs(", a, ")"]
  signum (Socket a) = Socket $ concat ["sign(", a, ")"]
  fromInteger = Socket . show . (fromInteger :: Integer -> Float)

instance Fractional (SocketReadOnly "float") where
  fromRational a = Socket $ show $
    (fromIntegral (numerator a) :: Float) / fromIntegral (denominator a)
  (/) (Socket a) (Socket b) = Socket $ concat [a, "/", b]

main_ = sub "void main() {" "}"

fromIxShader :: IxShader ctx '[] j a -> Either String TranslationUnit
fromIxShader = showLeft . parse . unlines . unDecl
  where showLeft = \case
          Left err  -> Left $ show err
          Right ast -> Right ast

toSrc :: Pretty a => a -> String
toSrc = show . pPrint

onlySrc :: IxShader ctx i j a -> String
onlySrc = unlines . unDecl

ixShaderSrc :: IxShader ctx '[] j a -> Either String String
ixShaderSrc = fmap toSrc . fromIxShader

putSrcLn :: forall ctx j a. IxShader ctx '[] j a -> IO ()
putSrcLn = either putStrLn (putStrLn . toSrc) . fromIxShader
