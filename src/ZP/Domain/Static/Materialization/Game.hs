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
data WorldObjs ts

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


-- Statically materialize world object

instance
  ( KnownNat x
  , KnownNat y
  , SMat p prop PropertyVL
  ) =>
  SMat p ('WorldObj @TypeLevel x y prop)
         WorldObjectVL where
  sMat p _ = do
    prop <- sMat p $ Proxy @prop
    let x = fromIntegral $ natVal $ Proxy @x
    let y = fromIntegral $ natVal $ Proxy @y
    pure $ WorldObj x y prop

-- Statically materialize world objects

instance
  SMat p (WorldObjs '[]) [WorldObjectVL] where
  sMat p _ = pure []

instance
  ( SMat p worldObj WorldObjectVL
  , SMat p (WorldObjs worldObjs) [WorldObjectVL]
  ) =>
  SMat p (WorldObjs (worldObj ': worldObjs))
         [WorldObjectVL] where
  sMat p _ = do
    worldObj  <- sMat p $ Proxy @worldObj
    worldObjs <- sMat p $ Proxy @(WorldObjs worldObjs)
    pure $ worldObj : worldObjs

-- Statically materialize game env

instance
  ( SMat p world WorldVL
  , SMat p (WorldObjs worldObjs) [WorldObjectVL]
  , SMat p (Props props) [PropertyVL]
  ) =>
  SMat p ('GameEnvironment @TypeLevel world props worldObjs)
         GameVL where
  sMat p _ = do
    world     <- sMat p $ Proxy @world
    worldObjs <- sMat p $ Proxy @(WorldObjs worldObjs)
    props     <- sMat p $ Proxy @(Props props)
    pure $ GameEnvironment world props worldObjs
