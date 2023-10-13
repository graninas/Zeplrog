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
data WorldObject (lvl :: Level) where
  WorldObj
    :: IntegerType lvl
    -> IntegerType lvl
    -> Property lvl
    -> WorldObject lvl


data Game (lvl :: Level) where
  GameEnvironment
    :: World lvl
    -- ^ Template world with objs to spawn
    -> [ Property lvl ]
    -- ^ Available static props to spawn objs from the world
    -> [ WorldObject lvl ]
    -- ^ World objects with pos to spawn
    -- -> [ Trigger lvl ]
    -> Game lvl

------ Short identifiers ------

type GameTL = Game 'TypeLevel
type GameVL = Game 'ValueLevel

type WorldObjectTL = WorldObject 'TypeLevel
type WorldObjectVL = WorldObject 'ValueLevel

