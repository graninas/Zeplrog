{-# LANGUAGE DataKinds #-}

module ZP.Domain.Dynamic.Model.Game where

import ZP.Prelude

import ZP.Domain.Dynamic.Model.Common
import ZP.Domain.Dynamic.Model.Property
import ZP.Domain.Dynamic.Model.Effect
import ZP.Domain.Dynamic.Model.Script
import ZP.Domain.Dynamic.Model.World

import qualified Data.Map as Map

data Game = Game
  { gWorld       :: World
  , gCells       :: [Int]     -- TODO: cells
  , gProps       :: Map.Map Essence Property
  , gEffTriggers :: [EffectTrigger]
  }
