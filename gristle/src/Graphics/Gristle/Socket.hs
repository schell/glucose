{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans            #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}
module Graphics.Gristle.Socket where


import           Data.List                 (intercalate)
import           Data.Promotion.Prelude
import           Data.Ratio                (denominator, numerator)
import           Graphics.Gristle.IxShader
import           Language.Haskell.TH
import           Prelude                   hiding (return, (>>), (>>=))

class KnownTypeSymbol a where
  typeSymbolVal :: Proxy a -> String

genKnownTypeSymbol :: TypeQ -> ExpQ -> DecsQ
genKnownTypeSymbol t s = [d|
  instance KnownTypeSymbol $t where
    typeSymbolVal _ = $s
  |]

-- | A socket is simply a place where you can stick an external expression
-- as a string. It's good for named uninitializeds, function application, all sorts of
-- stuff.
class Socketed a where
  unSocket :: a -> String
  socket   :: String -> a

genSocketed :: TypeQ -> ExpQ -> ExpQ -> DecsQ
genSocketed t un con = [d|
  instance Socketed $t where
    unSocket = $un
    socket = $con
  |]

instance Socketed () where
  unSocket = const ""
  socket = const ()

instance KnownTypeSymbol () where
  typeSymbolVal _ = "void"

call
  :: (Socketed a, Socketed b)
  => String
  -> a -> b
call fncstr a = socket $ concat [fncstr, "(", unSocket a, ")"]

call2
  :: (Socketed a, Socketed b, Socketed c)
  => String
  -> a -> b -> c
call2 fncstr a b =
  socket $ concat [fncstr, "(", unSocket a, ",", unSocket b, ")"]

call3
  :: (Socketed a, Socketed b, Socketed c, Socketed d)
  => String
  -> a -> b -> c -> d
call3 fncstr a b c =
  socket $ concat [fncstr, "(", unSocket a, ",", unSocket b, ",", unSocket c, ")"]

call4
  :: (Socketed a, Socketed b, Socketed c, Socketed d, Socketed e)
  => String
  -> a -> b -> c -> d -> e
call4 fncstr a b c d = socket $ concat [fncstr, "(", params, ")"]
  where params = intercalate "," [unSocket a, unSocket b, unSocket c, unSocket d]

callInfix
  :: (Socketed a, Socketed b, Socketed c)
  => String
  -> a -> b -> c
callInfix fncstr a b =
  socket $ concat ["(", unSocket a, fncstr, unSocket b, ")"]

toDefinition :: forall a. (Socketed a, KnownTypeSymbol a) => a -> String
toDefinition a = unwords [typeSymbolVal $ Proxy @a, unSocket a]

genNum :: TypeQ -> DecsQ
genNum typ = [d|
  instance Socketed $typ => Num $typ where
    (+) = callInfix "+"
    (-) = callInfix "-"
    (*) = callInfix "*"
    negate a = socket $ concat ["(-", unSocket a, ")"]
    abs    = call "abs"
    signum = call "sign"
    fromInteger = socket . show
 |]

genFractional :: TypeQ -> DecsQ
genFractional typ = [d|
  instance Socketed $typ => Fractional $typ where
    fromRational a = socket $ show $
      (fromIntegral (numerator a) :: Float) / fromIntegral (denominator a)
    (/) = callInfix "/"
  |]

genFloating :: TypeQ -> DecsQ
genFloating typ = [d|
  instance Socketed $typ => Floating $typ where
    pi = socket $ show (pi :: Float)
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
 |]

-- | Construct a new thing. Declares the thing w/o initialization.
define
  :: (Socketed a, KnownTypeSymbol a)
  => a
  -> IxShader ctx i i a
define a = nxt (toDefinition a ++ ";") a

stringDefinition :: (Socketed k, KnownTypeSymbol k) => k -> k -> String
stringDefinition k v = toDefinition k ++ " = " ++ unSocket v ++ ";"

-- | Construct a new assignable thing. Initializes it with another thing.
defineAs
  :: (Socketed a, KnownTypeSymbol a)
  => String
  -> a
  -> IxShader ctx i i a
defineAs s v =
  let k = socket s in nxt (stringDefinition k v) k

def
  :: (Socketed a, KnownTypeSymbol a)
  => String
  -> a
  -> IxShader ctx i i a
def = defineAs
