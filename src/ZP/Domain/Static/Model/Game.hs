{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZP.Domain.Static.Model.Game where

import ZP.Prelude
import GHC.TypeLits
import qualified Text.Show as T

import ZP.Domain.Static.Model.Common
import ZP.Domain.Static.Model.Property
import ZP.Domain.Static.Model.Effect
import ZP.Domain.Static.Model.Object
import ZP.Domain.Static.Model.World

------ Game and environment -----

data IconEssencePath (lvl :: Level) where
  IconPath :: EssencePath lvl -> IconEssencePath lvl

data PosEssencePath (lvl :: Level) where
  PosPath :: EssencePath lvl -> PosEssencePath lvl

data Game (lvl :: Level) where
  GameEnvironment
    :: World lvl
    -- ^ Template world with objs to spawn
    -> IconEssencePath lvl
    -- ^ Config: path to the icon prop
    -> PosEssencePath lvl
    -- ^ Config: path to the pos prop
    -> [ Property lvl ]
    -- ^ Available static props to spawn objs from the world
    -> [ Object lvl ]
    -- ^ Objects with pos to spawn
    -- -> [ Trigger lvl ]
    -> Game lvl

------ Short identifiers ------

type GameTL = Game 'TypeLevel
type GameVL = Game 'ValueLevel

type IconEssencePathTL = IconEssencePath 'TypeLevel
type IconEssencePathVL = IconEssencePath 'ValueLevel

type PosEssencePathTL = PosEssencePath 'TypeLevel
type PosEssencePathVL = PosEssencePath 'ValueLevel

