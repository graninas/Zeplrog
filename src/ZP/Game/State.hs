module ZP.Game.State where

import ZP.Prelude

import ZP.Types
import ZP.Gloss.Types
import ZP.Game.Types
import ZP.Game.Debug

import qualified Data.Map as Map

data AnimatedObject
  = AnimatedPath (TVar ActivePath)

type AnimatedObjects = Map.Map ObjectId AnimatedObject

data GameState = GameState
  { wndSizeVar        :: TVar GlossWindowSize
  , gridDimsVar       :: TVar GridDimensions
  , bareCellSizeVar   :: TVar BareCellSize
  , cellSpaceSizeVar  :: TVar CellSpaceSize
  , playerActorState  :: ActorState
  , levelVar          :: TVar Level
  , debugOptionsVar   :: TVar DebugOptions

  , objectIdCounterVar :: TVar ObjectId
  , animatedObjectsVar :: TVar AnimatedObjects
  }
