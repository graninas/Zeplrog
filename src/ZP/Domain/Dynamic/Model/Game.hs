{-# LANGUAGE DataKinds #-}

module ZP.Domain.Dynamic.Model.Game where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import ZP.Domain.Dynamic.Model.Common
import ZP.Domain.Dynamic.Model.Property
import ZP.Domain.Dynamic.Model.Effect
import ZP.Domain.Dynamic.Model.Script
import ZP.Domain.Dynamic.Model.Object
import ZP.Domain.Dynamic.Model.World

import qualified Data.Map as Map


data Game = Game
  { gWorld :: World
  -- ^ Game world with acting objects

  , gPropertyIdCounter :: TVar Int
  , gObjectIdCounter   :: TVar Int

  , gStaticProperties :: Map.Map StaticPropertyId (EssenceVL, PropertyVL)
  , gStaticEssences   :: Map.Map EssenceVL (StaticPropertyId, PropertyVL)
  -- ^ List of all template static properties

  , gObjects :: TVar (Map.Map ObjectId Object)
  -- ^ World objects

  -- , gProperties :: TVar (Map.Map PropertyId Property)
  -- ^ List of all dynamic properties

  -- TODO: rework triggers (not working now)
  -- , gEffTriggers :: [EffectTrigger]
  }
