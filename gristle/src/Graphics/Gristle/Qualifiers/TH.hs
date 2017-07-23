{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
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
module Graphics.Gristle.Qualifiers.TH where

import Data.Proxy (Proxy(..))

import Graphics.Gristle.Socket

data Uniform typ name = Uniform 
instance KnownTypeSymbol t => KnownTypeSymbol (Uniform t n) where
  typeSymbolVal _ = typeSymbolVal $ Proxy @t

data In typ name = In
instance KnownTypeSymbol t => KnownTypeSymbol (In t n) where
  typeSymbolVal _ = typeSymbolVal $ Proxy @t

data Out typ name = Out
instance KnownTypeSymbol t => KnownTypeSymbol (Out t n) where
  typeSymbolVal _ = typeSymbolVal $ Proxy @t

--newtype Const typ = Const { unConst :: typ }
--
--instance Socketed t => Socketed (Const t) where
--  unSocket = unSocket . unConst
--  socket = Const . socket
--
--instance KnownTypeSymbol t => KnownTypeSymbol (Const t) where
--  typeSymbolVal _ = typeSymbolVal $ Proxy @t


-- Read and write rules
-- type family ReadOnly a where
--   ReadOnly (Uniform t n) = t
--   ReadOnly (In t n)      = t
--   ReadOnly (Out t n)     = Error "Out values cannot be read."
--   ReadOnly (Const t)     = t
--   ReadOnly t             = t
-- 
-- type family WriteOnly a where
--   WriteOnly (Uniform t n) = Error "Uniform values can only be read."
--   WriteOnly (In t n)      = Error "In values can only be read."
--   WriteOnly (Out t n)     = t
--   WriteOnly (Const t)     = Error "Const values cannot be written to."
--   WriteOnly t             = t
-- 
-- class ReadWrite a where
--   mkReadable  :: a -> ReadOnly a
--   mkWriteable :: a -> WriteOnly a
-- 
-- genReadWrite :: TypeQ -> DecsQ
-- genReadWrite t = [d|
--   instance ReadWrite $t where
--     mkReadable  = id 
--     mkWriteable = id 
--   |]

