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

data Cell (lvl :: Level) where
  CellObjects :: Nat -> Nat -> [Essence lvl] -> Cell lvl

data Game (lvl :: Level) where
  GameEnvironment
    :: World lvl
    -> [ Cell lvl ]
    -> [ Property lvl ]
    -> [ Trigger lvl ]
    -> Game lvl

------ Short identifiers ------

type GameTL = Game 'TypeLevel
type GameVL = Game 'ValueLevel

type CellTL = Cell 'TypeLevel
type CellVL = Cell 'ValueLevel
