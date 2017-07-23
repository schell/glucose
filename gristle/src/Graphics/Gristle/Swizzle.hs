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
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans            #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}
module Graphics.Gristle.Swizzle
  ( Swizzled
  , swizzle
  , x
  , y
  , z
  , xy
  , xz
  , yz
  , xyz
  ) where

import           Data.Proxy                  (Proxy (..))
import           Data.Singletons.TypeLits
import           Prelude                     hiding (Read, return, (>>), (>>=))


import           Graphics.Gristle.Socket     as G
import           Graphics.Gristle.Types      as G

type family Swizzled a b where
  Swizzled 1 Xvec2 = Xfloat
  Swizzled 1 Xvec3 = Xfloat
  Swizzled 1 Xvec4 = Xfloat
  Swizzled 2 Xvec2 = Xvec2
  Swizzled 2 Xvec3 = Xvec2
  Swizzled 2 Xvec4 = Xvec2
  Swizzled 3 Xvec2 = Error "Swizzled error: vector field selection out of range"
  Swizzled 3 Xvec3 = Xvec3
  Swizzled 3 Xvec4 = Xvec3
  Swizzled 4 Xvec2 = Error "Swizzled error: vector field selection out of range"
  Swizzled 4 Xvec3 = Error "Swizzled error: vector field selection out of range"
  Swizzled 4 Xvec4 = Xvec4

swizzle
  :: forall (n :: Nat) a.
     ( Socketed a, Socketed (Swizzled n a)
     , KnownNat n
     )
  => String
  -> a
  -> Swizzled n a
swizzle s a = socket $ concat ["("
                              , unSocket a
                              , ")." ++ take (fromIntegral $ natVal $ Proxy @n) s
                              ]

x :: forall a.
     (Socketed a, Socketed (Swizzled 1 a))
  => a
  -> Swizzled 1 a
x = swizzle @1 "x"

y :: forall a.
     (Socketed a, Socketed (Swizzled 1 a))
  => a
  -> Swizzled 1 a
y = swizzle @1 "y"

z :: forall a.
     (Socketed a, Socketed (Swizzled 1 a))
  => a
  -> Swizzled 1 a
z = swizzle @1 "z"

xy :: forall a. 
     (Socketed a, Socketed (Swizzled 2 a))
  => a
  -> Swizzled 2 a
xy = swizzle @2 "xy"

xz :: forall a. 
     (Socketed a, Socketed (Swizzled 2 a))
  => a
  -> Swizzled 2 a
xz = swizzle @2 "xz"

yz :: forall a. 
     (Socketed a, Socketed (Swizzled 2 a))
  => a
  -> Swizzled 2 a
yz = swizzle @2 "yz"

xyz :: forall a. 
     (Socketed a, Socketed (Swizzled 3 a))
  => a
  -> Swizzled 3 a
xyz = swizzle @3 "xyz"
