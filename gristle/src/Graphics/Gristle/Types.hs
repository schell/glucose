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
module Graphics.Gristle.Types
  ( module Graphics.Gristle.Types
  , module G
  ) where


import           Prelude                      hiding (Ord (..), Read, return,
                                               (>>), (>>=))

import           Graphics.Gristle.Socket      as G
import           Graphics.Gristle.Types.Xbool as G
import           Graphics.Gristle.Function.ToParams

newtype Xvoid = Xvoid { unXvoid :: () }
instance Socketed Xvoid where
  unSocket = const "void"
  socket = const $ Xvoid ()
instance KnownTypeSymbol Xvoid where
  typeSymbolVal _ = "void"

--------------------------------------------------------------------------------
-- int, uint, float
--------------------------------------------------------------------------------
newtype Xint = Xint { unXint :: String }
$(genKnownTypeSymbol [t|Xint|] [e|"int"|])
$(genSocketed        [t|Xint|] [e|unXint|] [e|Xint|])
$(genNum             [t|Xint|])
$(genToParams        [t|Xint|])

--int :: Int -> Xint 
--int = Xint . show

newtype Xuint = Xuint { unXuint :: String }
$(genKnownTypeSymbol [t|Xuint|] [e|"uint"|])
$(genSocketed        [t|Xuint|] [e|unXuint|] [e|Xuint|])
$(genNum             [t|Xuint|])
$(genToParams        [t|Xuint|])

--uint :: Word -> Xuint
--uint = Xuint . show

newtype Xfloat = Xfloat { unXfloat :: String }
$(genKnownTypeSymbol [t|Xfloat|] [e|"float"|])
$(genSocketed        [t|Xfloat|] [e|unXfloat|] [e|Xfloat|])
$(genNum             [t|Xfloat|])
$(genFractional      [t|Xfloat|])
$(genFloating        [t|Xfloat|])
$(genToParams        [t|Xfloat|])

--float :: Float -> Xfloat
--float = Xfloat . show

--------------------------------------------------------------------------------
-- vec[2,3,4]
--------------------------------------------------------------------------------
newtype Xvec2 = Xvec2 { unXvec2 :: String }
$(genKnownTypeSymbol [t|Xvec2|] [e|"vec2"|])
$(genSocketed        [t|Xvec2|] [e|unXvec2|] [e|Xvec2|])
$(genNum             [t|Xvec2|])
$(genFractional      [t|Xvec2|])
$(genFloating        [t|Xvec2|])
$(genToParams        [t|Xvec2|])

--vec2 :: Float -> Float -> Xvec2
--vec2 

newtype Xvec3 = Xvec3 { unXvec3 :: String }
$(genKnownTypeSymbol [t|Xvec3|] [e|"vec3"|])
$(genSocketed        [t|Xvec3|] [e|unXvec3|] [e|Xvec3|])
$(genNum             [t|Xvec3|])
$(genFractional      [t|Xvec3|])
$(genFloating        [t|Xvec3|])
$(genToParams        [t|Xvec3|])

newtype Xvec4 = Xvec4 { unXvec4 :: String }
$(genKnownTypeSymbol [t|Xvec4|] [e|"vec4"|])
$(genSocketed        [t|Xvec4|] [e|unXvec4|] [e|Xvec4|])
$(genNum             [t|Xvec4|])
$(genFractional      [t|Xvec4|])
$(genFloating        [t|Xvec4|])
$(genToParams        [t|Xvec4|])


--------------------------------------------------------------------------------
-- bvec[2,3,4]
--------------------------------------------------------------------------------
newtype Xbvec2 = Xbvec2 { unXbvec2 :: String }
$(genKnownTypeSymbol [t|Xbvec2|] [e|"bvec2"|])
$(genSocketed        [t|Xbvec2|] [e|unXbvec2|] [e|Xbvec2|])
$(genToParams        [t|Xbvec2|])

newtype Xbvec3 = Xbvec3 { unXbvec3 :: String }
$(genKnownTypeSymbol [t|Xbvec3|] [e|"bvec3"|])
$(genSocketed        [t|Xbvec3|] [e|unXbvec3|] [e|Xbvec3|])
$(genToParams        [t|Xbvec3|])

newtype Xbvec4 = Xbvec4 { unXbvec4 :: String }
$(genKnownTypeSymbol [t|Xbvec4|] [e|"bvec4"|])
$(genSocketed        [t|Xbvec4|] [e|unXbvec4|] [e|Xbvec4|])
$(genToParams        [t|Xbvec4|])


--------------------------------------------------------------------------------
-- ivec[2,3,4]
--------------------------------------------------------------------------------
newtype Xivec2 = Xivec2 { unXivec2 :: String }
$(genKnownTypeSymbol [t|Xivec2|] [e|"ivec2"|])
$(genSocketed        [t|Xivec2|] [e|unXivec2|] [e|Xivec2|])
$(genNum             [t|Xivec2|])
$(genToParams        [t|Xivec2|])

newtype Xivec3 = Xivec3 { unXivec3 :: String }
$(genKnownTypeSymbol [t|Xivec3|] [e|"ivec3"|])
$(genSocketed        [t|Xivec3|] [e|unXivec3|] [e|Xivec3|])
$(genNum             [t|Xivec3|])
$(genToParams        [t|Xivec3|])

newtype Xivec4 = Xivec4 { unXivec4 :: String }
$(genKnownTypeSymbol [t|Xivec4|] [e|"ivec4"|])
$(genSocketed        [t|Xivec4|] [e|unXivec4|] [e|Xivec4|])
$(genNum             [t|Xivec4|])
$(genToParams        [t|Xivec4|])


--------------------------------------------------------------------------------
-- uvec[2,3,4]
--------------------------------------------------------------------------------
newtype Xuvec2 = Xuvec2 { unXuvec2 :: String }
$(genKnownTypeSymbol [t|Xuvec2|] [e|"uvec2"|])
$(genSocketed        [t|Xuvec2|] [e|unXuvec2|] [e|Xuvec2|])
$(genNum             [t|Xuvec2|])
$(genToParams        [t|Xuvec2|])

newtype Xuvec3 = Xuvec3 { unXuvec3 :: String }
$(genKnownTypeSymbol [t|Xuvec3|] [e|"uvec3"|])
$(genSocketed        [t|Xuvec3|] [e|unXuvec3|] [e|Xuvec3|])
$(genNum             [t|Xuvec3|])
$(genToParams        [t|Xuvec3|])

newtype Xuvec4 = Xuvec4 { unXuvec4 :: String }
$(genKnownTypeSymbol [t|Xuvec4|] [e|"uvec4"|])
$(genSocketed        [t|Xuvec4|] [e|unXuvec4|] [e|Xuvec4|])
$(genNum             [t|Xuvec4|])
$(genToParams        [t|Xuvec4|])


--------------------------------------------------------------------------------
-- mat2x[2,3,4]
--------------------------------------------------------------------------------
newtype Xmat2 = Xmat2 { unXmat2 :: String }
$(genKnownTypeSymbol [t|Xmat2|] [e|"mat2"|])
$(genSocketed        [t|Xmat2|] [e|unXmat2|] [e|Xmat2|])
$(genToParams        [t|Xmat2|])
type Xmat2x2 = Xmat2

newtype Xmat2x3 = Xmat2x3 { unXmat2x3 :: String }
$(genKnownTypeSymbol [t|Xmat2x3|] [e|"mat2x3"|])
$(genSocketed        [t|Xmat2x3|] [e|unXmat2x3|] [e|Xmat2x3|])
$(genToParams        [t|Xmat2x3|])

newtype Xmat2x4 = Xmat2x4 { unXmat2x4 :: String }
$(genKnownTypeSymbol [t|Xmat2x4|] [e|"mat2x4"|])
$(genSocketed        [t|Xmat2x4|] [e|unXmat2x4|] [e|Xmat2x4|])
$(genToParams        [t|Xmat2x4|])


--------------------------------------------------------------------------------
-- mat3x[2,3,4]
--------------------------------------------------------------------------------
newtype Xmat3x2 = Xmat3x2 { unXmat3x2 :: String }
$(genKnownTypeSymbol [t|Xmat3x2|] [e|"mat3x2"|])
$(genSocketed        [t|Xmat3x2|] [e|unXmat3x2|] [e|Xmat3x2|])
$(genToParams        [t|Xmat3x2|])

newtype Xmat3 = Xmat3 { unXmat3 :: String }
$(genKnownTypeSymbol [t|Xmat3|] [e|"mat3"|])
$(genSocketed        [t|Xmat3|] [e|unXmat3|] [e|Xmat3|])
$(genToParams        [t|Xmat3|])
type Xmat3x3 = Xmat3

newtype Xmat3x4 = Xmat3x4 { unXmat3x4 :: String }
$(genKnownTypeSymbol [t|Xmat3x4|] [e|"mat3x4"|])
$(genSocketed        [t|Xmat3x4|] [e|unXmat3x4|] [e|Xmat3x4|])
$(genToParams        [t|Xmat3x4|])

--------------------------------------------------------------------------------
-- mat4x[2,3,4]
--------------------------------------------------------------------------------
newtype Xmat4x2 = Xmat4x2 { unXmat4x2 :: String }
$(genKnownTypeSymbol [t|Xmat4x2|] [e|"mat4x2"|])
$(genSocketed        [t|Xmat4x2|] [e|unXmat4x2|] [e|Xmat4x2|])
$(genToParams        [t|Xmat4x2|])

newtype Xmat4x3 = Xmat4x3 { unXmat4x3 :: String }
$(genKnownTypeSymbol [t|Xmat4x3|] [e|"mat4x3"|])
$(genSocketed        [t|Xmat4x3|] [e|unXmat4x3|] [e|Xmat4x3|])
$(genToParams        [t|Xmat4x3|])

newtype Xmat4 = Xmat4 { unXmat4 :: String }
$(genKnownTypeSymbol [t|Xmat4|] [e|"mat4"|])
$(genSocketed        [t|Xmat4|] [e|unXmat4|] [e|Xmat4|])
$(genToParams        [t|Xmat4|])
type Xmat4x4 = Xmat4
