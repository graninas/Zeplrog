{-# LANGUAGE DataKinds #-}

module ZP.Domain.Dynamic.Model.Game where

import ZP.Prelude

import ZP.Domain.Dynamic.Model.Common
import ZP.Domain.Dynamic.Model.Property
import ZP.Domain.Dynamic.Model.Effect

import qualified Data.Map as Map

data Game = Game
  { props       :: Map.Map Essence Property
  , effTriggers :: [EffectTrigger]
  }
