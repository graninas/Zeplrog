module ZP.Domain.Dynamic.Model.World where

import ZP.Prelude

import ZP.Domain.Dynamic.Model.Common
import ZP.Domain.Dynamic.Model.Object

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V


type ActiveObjects = Set.Set ObjectId

-- | World structure.
data World = World
  { wWorldDims :: (Int, Int)
  -- ^ World dimensions of fixed size.
  , wWorldGrid :: V.Vector (V.Vector (TVar ActiveObjects))
  -- ^ World grid with references to objects.
  }
