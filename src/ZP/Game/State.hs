module ZP.Game.State where

import ZP.Prelude

import ZP.Types
import ZP.Gloss.Types

import qualified Data.Map as Map


data GameState = GameState
  { wndSizeVar        :: TVar GlossWindowSize
  , gridDimsVar       :: TVar GridDimensions
  , bareCellSizeVar   :: TVar BareCellSize
  , cellSpaceSizeVar  :: TVar CellSpaceSize
  , playerPosVar      :: TVar PlayerPosition
  , levelVar          :: TVar Level
  }
