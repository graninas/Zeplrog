{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Game where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization.Materializer
import ZP.Domain.Static.Materialization.Common
import ZP.Domain.Static.Materialization.Effect
import ZP.Domain.Static.Materialization.Property
import ZP.Domain.Static.Materialization.Object
import ZP.Domain.Static.Materialization.World

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


data Triggs ts

-- Statically materialize triggers

instance
  SMat () (Triggs '[]) [TriggerVL] where
  sMat () _ = pure []

instance
  ( SMat () trig TriggerVL
  , SMat () (Triggs triggs) [TriggerVL]
  ) =>
  SMat () (Triggs (trig ': triggs))
         [TriggerVL] where
  sMat () _ = do
    trig  <- sMat () $ Proxy @trig
    triggs <- sMat () $ Proxy @(Triggs triggs)
    pure $ trig : triggs

-- Statically materialize game env

instance
  ( SMat () world WorldVL
  , SMat () (Objs objs) [ObjectVL]
  , SMat () (Props props) [PropertyVL]
  , SMat () pathToIcon EssencePathVL
  , SMat () pathToPos EssencePathVL
  ) =>
  SMat () ('GameEnvironment @TypeLevel
            world
            ('IconPath @TypeLevel pathToIcon)
            ('PosPath  @TypeLevel pathToPos)
            props
            objs
          )
         GameVL where
  sMat () _ = do
    world      <- sMat () $ Proxy @world
    pathToIcon <- sMat () $ Proxy @pathToIcon
    pathToPos  <- sMat () $ Proxy @pathToPos
    objs       <- sMat () $ Proxy @(Objs objs)
    props      <- sMat () $ Proxy @(Props props)
    pure $ GameEnvironment
      world
      (IconPath pathToIcon)
      (PosPath pathToPos)
      props
      objs
