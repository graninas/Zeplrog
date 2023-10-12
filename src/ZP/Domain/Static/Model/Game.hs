{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZP.Domain.Static.Model.Game where

import ZP.Prelude
import GHC.TypeLits
import qualified Text.Show as T

import ZP.Domain.Static.Model.Common
import ZP.Domain.Static.Model.Property
import ZP.Domain.Static.Model.Effect
import ZP.Domain.Static.Model.World

------ Game and environment -----

-- | Objects in the world.
-- No static type-level version.
data Cell where
  CellObject :: (Int, Int) -> PropertyVL -> Cell


data Game (lvl :: Level) where
  GameEnvironment
    :: World lvl              -- ^ Template world
    -> [ Cell ]               -- ^ World objects
    -> [ Property lvl ]       -- ^ Template static properties
    -> [ Trigger lvl ]
    -> Game lvl

------ Short identifiers ------

type GameTL = Game 'TypeLevel
type GameVL = Game 'ValueLevel

