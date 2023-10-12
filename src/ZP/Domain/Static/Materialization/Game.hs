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


data Triggs ts

-- Statically materialize triggers

instance
  SMat p (Triggs '[]) [TriggerVL] where
  sMat p _ = pure []

instance
  ( SMat p trig TriggerVL
  , SMat p (Triggs triggs) [TriggerVL]
  ) =>
  SMat p (Triggs (trig ': triggs))
      [TriggerVL] where
  sMat p _ = do
    trig  <- sMat p $ Proxy @trig
    triggs <- sMat p $ Proxy @(Triggs triggs)
    pure $ trig : triggs

-- Statically materialize game env

instance
  ( SMat p world WorldVL
  , SMat p (Props props) [(EssenceVL, PropertyVL)]
  , SMat p (Triggs triggs) [TriggerVL]
  ) =>
  SMat p ('GameEnvironment @TypeLevel world cells props triggs)
      (Game 'ValueLevel) where
  sMat p _ = do
    world  <- sMat p $ Proxy @world
    props  <- sMat p $ Proxy @(Props props)
    triggs <- sMat p $ Proxy @(Triggs triggs)
    let props' = map snd props
    pure $ GameEnvironment world [] props' triggs   -- TODO: objects
