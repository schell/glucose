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
module Graphics.Gristle.Function
  ( module Graphics.Gristle.Function
  , (:++)
  ) where

import           Data.List                          (intercalate)
import           Data.Promotion.Prelude
import           Data.Singletons.TypeLits
import           Prelude                            hiding (Read, return, (>>),
                                                     (>>=))

import           Graphics.Gristle.Function.ToParams
import           Graphics.Gristle.IxShader
import           Graphics.Gristle.Socket

--------------------------------------------------------------------------------
-- Defining and calling functions
-------------------------------------------------------------------------------
funcReturnType :: forall t ctx i. (KnownTypeSymbol t) => IxShader ctx i i ()
funcReturnType = nxt_ $ typeSymbolVal $ Proxy @t

funcName :: forall name ctx i. (KnownSymbol name) => IxShader ctx i i ()
funcName = nxt_ $ symbolVal $ Proxy @name

funcParams :: ToParams ps => ps -> IxShader ctx i i ()
funcParams ps = nxt_ $ "(" ++ intercalate ", " (toParams ps) ++ ")"

returnValue
  :: (Socketed a, KnownTypeSymbol a)
  => a -> IxShader ctx i i a 
returnValue a = nxt (unwords ["return", unSocket a, ";"]) a

funcCall
  :: forall name t ps. (KnownSymbol name, Socketed t, ToParams ps)
  => ps
  -> t
funcCall ps = socket $ unwords [ symbolVal $ Proxy @name
                               , "("
                               , intercalate ", " $ toNames ps
                               , ")"
                               ]

data Function rtype fname ps = Function

type IxFunction ctx i rtype fname ps =
  IxShader ctx i (i :++ '[Function rtype fname ps]) (ps -> rtype)

func
  :: forall fname rtype ps ctx i.
     (ToParams ps, KnownTypeSymbol rtype, Socketed rtype, KnownSymbol fname)
  => ps
  -> (ps -> IxShader ctx i i rtype)
  -> IxFunction ctx i rtype fname ps
func ps f = do
  nxt_ ""
  funcReturnType @rtype
  funcName @fname
  funcParams ps
  sub_ "{" "}" $ f ps
  acc "" (Function @rtype @fname @ps) ()
  nxt_ ""
  return $ funcCall @fname

type Main = Function () "main" ()

