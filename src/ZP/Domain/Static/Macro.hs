{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZP.Domain.Static.Macro where

import ZP.Prelude
import GHC.TypeLits
import qualified Text.Show as T

import ZP.Domain.Static.Model.Common
import ZP.Domain.Static.Model.Property
import ZP.Domain.Static.Model.Effect
import ZP.Domain.Static.Model.World
import ZP.Domain.Static.Model.Script


------ Macro -----

-- | Macros for building type-level game
data MacroGame where
  MGame
    :: WorldTL
    -> [Macro]
    -> MacroGame

-- | Specific macro commands
data Macro where
  -- | Build the world using the map and properties for map objects
  UseWorld    :: WorldTL -> [PropertyTL] -> Macro
  -- | Use triggers
  UseTriggers :: [TriggerTL] -> Macro
  -- | Displace an object in the world
  PlaceObj    :: Nat -> Nat -> PropertyTL -> Macro

