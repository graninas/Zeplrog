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
data Objs ts

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


-- Statically materialize object

instance
  ( KnownNat x
  , KnownNat y
  , SMat p prop PropertyVL
  ) =>
  SMat p ('Obj @TypeLevel x y prop)
         ObjectVL where
  sMat p _ = do
    prop <- sMat p $ Proxy @prop
    let x = fromIntegral $ natVal $ Proxy @x
    let y = fromIntegral $ natVal $ Proxy @y
    pure $ Obj x y prop

-- Statically materialize objects

instance
  SMat p (Objs '[]) [ObjectVL] where
  sMat p _ = pure []

instance
  ( SMat p obj ObjectVL
  , SMat p (Objs objs) [ObjectVL]
  ) =>
  SMat p (Objs (obj ': objs))
         [ObjectVL] where
  sMat p _ = do
    obj  <- sMat p $ Proxy @obj
    objs <- sMat p $ Proxy @(Objs objs)
    pure $ obj : objs

-- Statically materialize game env

instance
  ( SMat p world WorldVL
  , SMat p (Objs objs) [ObjectVL]
  , SMat p (Props props) [PropertyVL]
  , SMat p (Essences pathToIcon) [EssenceVL]
  ) =>
  SMat p ('GameEnvironment @TypeLevel world pathToIcon props objs)
         GameVL where
  sMat p _ = do
    world      <- sMat p $ Proxy @world
    pathToIcon <- sMat p $ Proxy @(Essences pathToIcon)
    objs       <- sMat p $ Proxy @(Objs objs)
    props      <- sMat p $ Proxy @(Props props)
    pure $ GameEnvironment world pathToIcon props objs
