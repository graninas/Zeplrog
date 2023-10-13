module ZP.Domain.Dynamic.Model.World where

import ZP.Prelude

import ZP.Domain.Dynamic.Model.Common
import ZP.Domain.Dynamic.Model.Object

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V


type ActiveObjects = Set.Set ObjectId

-- TODO: use mutable vectors

-- type Row  = V.Vector (TVar ActiveObjects)
type Row  = V.Vector Char
type Rows = V.Vector Row

-- | World structure.
data World = World
  { wWorldDims :: (Int, Int)
  -- ^ World dimensions of fixed size.
  , wWorldGrid :: Rows
  -- ^ World grid with references to objects.
  }
