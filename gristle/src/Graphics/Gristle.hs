{-# LANGUAGE ConstraintKinds       #-}
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
  , module G
  , (&&&)
  , module Prelude
  ) where


import           Control.Arrow               ((&&&), (>>>))
import           Data.List                   (intercalate)
import           Data.Promotion.Prelude.List ((:++))
import           Data.Promotion.Prelude.Num
import           Data.Proxy
import           Data.Ratio                  (denominator, numerator)
import           Data.Singletons.TypeLits
import           Graphics.Gristle.Function   as G
import           Graphics.Gristle.IxShader   as G
import           Graphics.Gristle.Ops.Mult   as G
import           Graphics.Gristle.Qualifiers as G
import           Graphics.Gristle.Socket     as G
import           Graphics.Gristle.Swizzle    as G
import           Graphics.Gristle.Types      as G
import           Prelude                     hiding (Eq (..), Ord (..), fail,
                                              fromInteger, fromRational, length,
                                              return, (<), (>>), (>>=))

fromInteger :: Integer -> Xint
fromInteger = Xint . show

fromRational :: Rational -> Xfloat
fromRational =
  (numerator &&& denominator) >>> \(n, d) ->
    Xfloat $ show (fromIntegral n / fromIntegral d :: Float)

infixr 1 .=
(.=)
  :: forall a b i ctx.
     (Socketed a, Socketed b)
  => a -> b
  -> IxShader ctx i i ()
(.=) a b = nxt_ $ unwords [unSocket a, "=", unSocket b ++ ";"]


smoothstep :: (Socketed a, Socketed b, Socketed c, Socketed d) => a -> b -> c -> d
smoothstep = call3 "smoothstep"

step :: (Socketed a, Socketed b, Socketed c) => a -> b -> c
step = call2 "step"

mkvec2 :: Xfloat -> Xfloat -> Xvec2
mkvec2 = call2 "vec2"

mkvec3 :: Xfloat -> Xfloat -> Xfloat -> Xvec3
mkvec3 = call3 "vec3"

mkvec4 :: Xfloat -> Xfloat -> Xfloat -> Xfloat -> Xvec4
mkvec4 = call4 "vec4"

toInt :: Xfloat -> Xint
toInt = call "int"

toFloat :: Xint -> Xfloat
toFloat = call "float"

float :: Float -> Xfloat
float = Xfloat . show

int :: Int -> Xint
int = Xint . show

type family LengthOf a where
  LengthOf Xfloat = Xfloat
  LengthOf Xvec2  = Xfloat
  LengthOf Xvec3  = Xfloat
  LengthOf Xvec4  = Xfloat
  LengthOf a      = Error '(a, "Cannot call length on this type.")

length :: (Socketed a, Socketed (LengthOf a)) => a -> LengthOf a
length = call "length"

type family VectOf (n :: Nat) v where
  VectOf 1 Xfloat = Xfloat
  VectOf 2 Xfloat = Xvec2
  VectOf 3 Xfloat = Xvec3
  VectOf 4 Xfloat = Xvec4

type family CompType v where
  CompType Xfloat = Xfloat
  CompType Xvec2  = Xfloat
  CompType Xvec3  = Xfloat
  CompType Xvec4  = Xfloat

type family NumComps v where
  NumComps Xfloat = 1
  NumComps Xvec2  = 2
  NumComps Xvec3  = 3
  NumComps Xvec4  = 4

type family CompCat as bs where
  CompCat as bs = VectOf (NumComps as :+ NumComps bs) (CompType as)

infixr 5 .:
(.:)
  :: forall a b.
     ( KnownTypeSymbol (CompCat a b)
     , Socketed a, Socketed b, Socketed (CompCat a b)
     )
  => a -> b -> CompCat a b
(.:) = call2 (typeSymbolVal $ Proxy @(CompCat a b))

type IsGLContext ctx = (HasContext ctx, KnownSymbol (GLFragName ctx))

main_
  :: forall (ctx :: GLContext) i a.
  IxShader ctx i i a -> IxShader ctx i (i :++ '[Main]) ()
main_ f = void $ func @"main" () $ const $ void f

infixr 5 +=
(+=) :: Socketed a => a -> a -> a
(+=) = callInfix "+="

infixr 5 -=
(-=) :: Socketed a => a -> a -> a
(-=) = callInfix "-="


for
  :: (Socketed a, KnownTypeSymbol a)
  => (String, a)
  -> (a -> (Xbool, a))
  -> (a -> IxShader ctx i i b)
  -> IxShader ctx i i b
for (name, v) fi f = do
  let k = socket name
      (itill, iinc) = fi k
  nxt_ $ unwords [ "for ("
                 , stringDefinition k v
                 , intercalate "; " [ unSocket itill
                                    , unSocket iinc
                                    ]
                 , ")"
                 ]
  sub "{" "}" $ f k

vertex
  :: forall (ctx :: GLContext).
  IxShader ctx '[] '[ Uniform Xvec2 "u_resolution"
                    , Out Xvec4 "gl_Position"
                    , Function Xint "myFunc" (Xint, Xint)
                    , Main
                    ] ()
vertex = do
  res <- uniform_
  pos <- gl_Position

  myFunc <- func (Xint "a", Xint "b") $ \(a, b) -> returnValue $ a + b

  main_ $ do
    unless (1 == 1) $ do
      _ <- def "xxx" 1
      _ <- def "yyy" $ length pos
      return ()
    fa   <- def "a" 1.0
    fb   <- def "b" 2.0
    v4   <- def "v4" $ fa .: fb .: mkvec2 3.0 4.0
    v4   .= mkvec2 3.0 4.0 .: fa .: fb
    v4   .= mkvec4 1.0 2.0 3.0 4.0
    xwhy <- def "exwhy" $ fa .: fb
    x xwhy .= 0.0
    pa   <- def "paramA" 0.0
    pb   <- def "paramB" 1.0
    c    <- def "c" $ myFunc (toInt pa, toInt pb)
    c    .= 5
    c    .= x res
    for ("i", 0) ((< 5) &&& (+= 1)) $ \i -> pos .= mkvec4 (toFloat i) 2.0 3.0 4.0
