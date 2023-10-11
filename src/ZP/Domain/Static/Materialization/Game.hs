{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Game where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization.Materializer
import ZP.Domain.Static.Materialization.Common
import ZP.Domain.Static.Materialization.Effect
import ZP.Domain.Static.Materialization.Property
import ZP.Domain.Static.Materialization.World

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


data Props ps
data Triggs ts

-- Statically materialize triggers

instance
  SMat p (Triggs '[]) [Trigger 'ValueLevel] where
  sMat p _ = pure []

instance
  ( SMat p trig (Trigger 'ValueLevel)
  , SMat p (Triggs triggs) [Trigger 'ValueLevel]
  ) =>
  SMat p (Triggs (trig ': triggs))
      [Trigger 'ValueLevel] where
  sMat p _ = do
    trig  <- sMat p $ Proxy @trig
    triggs <- sMat p $ Proxy @(Triggs triggs)
    pure $ trig : triggs

-- Statically materialize props

instance
  SMat p (Props '[]) [(Essence 'ValueLevel, Property 'ValueLevel)] where
  sMat p _ = pure []

instance
  ( SMat p prop (Essence 'ValueLevel, Property 'ValueLevel)
  , SMat p (Props props) [(Essence 'ValueLevel, Property 'ValueLevel)]
  ) =>
  SMat p (Props (prop ': props))
      [(Essence 'ValueLevel, Property 'ValueLevel)] where
  sMat p _ = do
    prop  <- sMat p $ Proxy @prop
    props <- sMat p $ Proxy @(Props props)
    pure $ prop : props

-- Statically materialize game env

instance
  ( SMat p world (World 'ValueLevel)
  , SMat p (Props props) [(Essence 'ValueLevel, Property 'ValueLevel)]
  , SMat p (Triggs triggs) [Trigger 'ValueLevel]
  ) =>
  SMat p ('GameEnvironment @TypeLevel world cells props triggs)
      (Game 'ValueLevel) where
  sMat p _ = do
    world  <- sMat p $ Proxy @world
    props  <- sMat p $ Proxy @(Props props)
    triggs <- sMat p $ Proxy @(Triggs triggs)
    let props' = map snd props
    pure $ GameEnvironment world [] props' triggs   -- TODO: objects
