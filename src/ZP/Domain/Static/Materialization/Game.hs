{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Game where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization.Materializer
import ZP.Domain.Static.Materialization.Common
import ZP.Domain.Static.Materialization.Effect
import ZP.Domain.Static.Materialization.Property

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


data Props ps
data Triggs ts

-- Statically materialize triggers

instance
  SMat (Triggs '[]) [Trigger 'ValueLevel] where
  sMat _ = pure []

instance
  ( SMat trig (Trigger 'ValueLevel)
  , SMat (Triggs triggs) [Trigger 'ValueLevel]
  ) =>
  SMat (Triggs (trig ': triggs))
      [Trigger 'ValueLevel] where
  sMat _ = do
    trig  <- sMat $ Proxy @trig
    triggs <- sMat $ Proxy @(Triggs triggs)
    pure $ trig : triggs

-- Statically materialize props

instance
  SMat (Props '[]) [(Essence 'ValueLevel, Property 'ValueLevel)] where
  sMat _ = pure []

instance
  ( SMat prop (Essence 'ValueLevel, Property 'ValueLevel)
  , SMat (Props props) [(Essence 'ValueLevel, Property 'ValueLevel)]
  ) =>
  SMat (Props (prop ': props))
      [(Essence 'ValueLevel, Property 'ValueLevel)] where
  sMat _ = do
    prop  <- sMat $ Proxy @prop
    props <- sMat $ Proxy @(Props props)
    pure $ prop : props

-- Statically materialize game env

instance
  ( SMat (Props props) [(Essence 'ValueLevel, Property 'ValueLevel)]
  , SMat (Triggs triggs) [Trigger 'ValueLevel]
  ) =>
  SMat ('GameEnvironment @TypeLevel props triggs)
      (Game 'ValueLevel) where
  sMat _ = do
    props  <- sMat $ Proxy @(Props props)
    triggs <- sMat $ Proxy @(Triggs triggs)
    let props' = map snd props
    pure $ GameEnvironment props' triggs
