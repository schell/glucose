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
module Graphics.Gristle.Qualifiers
  ( module Graphics.Gristle.Qualifiers
  , module G
  ) where


import           Data.Promotion.Prelude         hiding (Const)
import           Data.Singletons.TypeLits
import           Prelude                        hiding (Read, return, (>>),
                                                 (>>=))

import           Graphics.Gristle.Function      
import           Graphics.Gristle.IxShader      
import           Graphics.Gristle.Qualifiers.TH as G
import           Graphics.Gristle.Types         

--------------------------------------------------------------------------------
-- Read write stuff (for all types) 
--------------------------------------------------------------------------------
--  $(genReadWrite [t|Xbool|] )
--  $(genReadWrite [t|Xint|]  )
--  $(genReadWrite [t|Xuint|] )
--  $(genReadWrite [t|Xfloat|])
--  
--  $(genReadWrite [t|Xvec2|]) 
--  $(genReadWrite [t|Xvec3|])
--  $(genReadWrite [t|Xvec4|])
--  
--  $(genReadWrite [t|Xbvec2|])
--  $(genReadWrite [t|Xbvec3|])
--  $(genReadWrite [t|Xbvec4|])
--  
--  $(genReadWrite [t|Xivec2|])
--  $(genReadWrite [t|Xivec3|])
--  $(genReadWrite [t|Xivec4|])
--  
--  $(genReadWrite [t|Xuvec2|])
--  $(genReadWrite [t|Xuvec3|])
--  $(genReadWrite [t|Xuvec4|])
--  
--  $(genReadWrite [t|Xmat2|]  )
--  $(genReadWrite [t|Xmat2x3|])
--  $(genReadWrite [t|Xmat2x4|])
--  
--  $(genReadWrite [t|Xmat3|]  )
--  $(genReadWrite [t|Xmat3x2|])
--  $(genReadWrite [t|Xmat3x4|])
--  
--  $(genReadWrite [t|Xmat4|]  )
--  $(genReadWrite [t|Xmat4x2|])
--  $(genReadWrite [t|Xmat4x3|])
--
--
--instance ReadWrite t => ReadWrite (Uniform t n) where
--  mkReadable = unUniform
--  mkWriteable = error "Uniform not writeable"
--  -- this looks scary but because of WriteOnly this function cannot be called
--  -- by construction (for the same reason it can't really be defined)
--
--instance ReadWrite et => ReadWrite (In t n) where
--  mkReadable = unIn
--  mkWriteable = error "In not writeable" 
--  
--instance ReadWrite t => ReadWrite (Out t n) where
--  mkReadable = error "Out not readable"
--  mkWriteable = unOut
--
--instance ReadWrite t => ReadWrite (Const t) where
--  mkReadable = unConst 
--  mkWriteable = error "Const not writeable"
--------------------------------------------------------------------------------
-- Program-level in/out bindings
--------------------------------------------------------------------------------
class Binding a t where
  getVertexBinding  :: t
  getUniformBinding :: t

instance KnownSymbol b => Binding (Uniform a b) (Maybe String) where
  getVertexBinding = Nothing
  getUniformBinding = Just $ symbolVal $ Proxy @b

instance KnownSymbol b => Binding (In a b) (Maybe String) where
  getVertexBinding = Just $ symbolVal $ Proxy @b
  getUniformBinding = Nothing

instance Binding (Out a b) (Maybe String) where
  getVertexBinding = Nothing
  getUniformBinding = Nothing

instance Binding (Function a b c) (Maybe String) where
  getVertexBinding = Nothing
  getUniformBinding = Nothing

instance Binding '[] [t] where
  getVertexBinding = []
  getUniformBinding = []

instance (Binding a t, Binding as [t]) => Binding (a ': as) [t] where
  getVertexBinding  = getVertexBinding  @a : getVertexBinding  @as
  getUniformBinding = getUniformBinding @a : getUniformBinding @as

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

uniform_
  :: forall t name ts ctx. (KnownSymbol name, Socketed t, KnownTypeSymbol t)
  => IxShader ctx ts (ts :++ '[Uniform t name]) t
uniform_ = acc decls (Uniform @t @name) u
  where
    u = socket $ symbolVal $ Proxy @name
    decls = unwords ["uniform", toDefinition u, ";"]

in_
  :: forall t name ts ctx.
     (HasContext ctx, KnownSymbol name, Socketed t, KnownTypeSymbol t)
  => IxShader ctx ts (ts :++ '[In t name]) t
in_ = acc decls (In @t @name) i
  where
    i   = socket $ symbolVal $ Proxy @name
    dec = case getCtx @ctx of
      OpenGLContext -> "in"
      WebGLContext  -> "attribute"
    decls = unwords [dec, toDefinition i, ";"]

out_
  :: forall t name ts ctx.
     (HasContext ctx, KnownSymbol name, Socketed t, KnownTypeSymbol t)
  => IxShader ctx ts (ts :++ '[Out t name]) t
out_ = acc decls (Out @t @name) o
  where
    o   = socket $ symbolVal $ Proxy @name
    dec = case getCtx @ctx of
      OpenGLContext -> "out"
      WebGLContext  -> "varying"
    decls = unwords [dec, toDefinition o, ";"]

gl_Position
  :: forall ts ctx.
  IxShader ctx ts (ts :++ '[Out Xvec4 "gl_Position"]) Xvec4
gl_Position = acc [] (Out @Xvec4 @"gl_Position") $ Xvec4 "gl_Position"

type family GLFragName (a :: GLContext) where
  GLFragName 'OpenGLContext = "fragColor"
  GLFragName 'WebGLContext  = "gl_FragColor"

gl_FragColor
  :: forall ctx ts. (HasContext ctx, KnownSymbol (GLFragName ctx))
  => IxShader ctx ts (ts :++ '[Out Xvec4 (GLFragName ctx)]) Xvec4
gl_FragColor = acc decls (Out @Xvec4 @(GLFragName ctx)) o
  where o = Xvec4 $ symbolVal $ Proxy @(GLFragName ctx)
        decls = case getCtx @ctx of
          OpenGLContext -> unwords ["out", toDefinition o, ";"]
          _             -> []

gl_FragCoord :: Xvec4
gl_FragCoord = Xvec4 "gl_FragCoord"
