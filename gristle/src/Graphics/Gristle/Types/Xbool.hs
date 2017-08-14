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
module Graphics.Gristle.Types.Xbool where


import           Prelude                 hiding (Ord (..), Read, return, (>>),
                                          (>>=), Eq (..))

import           Graphics.Gristle.Socket
import           Graphics.Gristle.Function.ToParams
import           Graphics.Gristle.IxShader


newtype Xbool = Xbool { unXbool :: String }
$(genKnownTypeSymbol [t|Xbool|] [e|"bool"|])
$(genSocketed        [t|Xbool|] [e|unXbool|] [e|Xbool|])
$(genToParams        [t|Xbool|])

ifThenElse :: Xbool -> IxShader ctx i i () -> IxShader ctx i i () -> IxShader ctx i i ()
ifThenElse x a b = do
  nxt_ $ "if (" ++ unSocket x ++ ")"
  sub_ "{" "}" a
  sub_ "else {" "}" b

when :: Xbool -> IxShader ctx i i () -> IxShader ctx i i ()
when x a = do
  nxt_ $ "if (" ++ unSocket x ++ ")"
  sub_ "{" "}" a

unless :: Xbool -> IxShader ctx i i () -> IxShader ctx i i ()
unless x a = do
  nxt_ $ "if (! " ++ unSocket x ++ ")"
  sub_ "{" "}" a

