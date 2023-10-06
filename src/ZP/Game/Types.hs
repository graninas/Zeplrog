module ZP.Game.Types where

import ZP.Prelude

import ZP.Types
import Graphics.Gloss

import qualified Data.Map as Map

data ActorGoal = FindExit
type ActorPath = [CellIdxs]

newtype ObjectId = ObjectId Int
  deriving (Show, Eq, Ord)

data PathDisplay = PathIsBlinking Picture Int
data ActivePath = ActivePath ObjectId ActorPath (Maybe PathDisplay)

data ObservingResult
  = PathFound (TVar ActivePath)

data ActorActivity
  = Idling Int
  | Observing Bool Int (Maybe ObservingResult)
  | FollowingPath (TVar ActivePath)


data ActorState = ActorState
  { goalVar            :: TVar ActorGoal
  , currentActivityVar :: TVar ActorActivity

  , currentPosVar   :: TVar CellIdxs
  , currentShapeVar :: TVar Picture

  , currentPathPointShapeVar :: TVar Picture
  }
