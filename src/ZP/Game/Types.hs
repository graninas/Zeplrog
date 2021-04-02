module ZP.Game.Types where

import ZP.Prelude

import ZP.Types
import Graphics.Gloss

import qualified Data.Map as Map

data Goal = FindExit


type ActorPath = [CellIdxs]

data PathDisplay
  = PathIsBlinking Int
  | PathIsInvisible

data ActorState = ActorState
  { goalVar         :: TVar Goal
  , currentPosVar   :: TVar CellIdxs
  , currentPathVar  :: TVar ActorPath
  , currentShapeVar :: TVar Picture

  , currentPathPointShapeVar :: TVar Picture
  , currentPathDisplayVar :: TVar PathDisplay
  , staticShape :: Picture
  }
