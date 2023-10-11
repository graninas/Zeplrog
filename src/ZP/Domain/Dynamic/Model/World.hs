module ZP.Domain.Dynamic.Model.World where

import ZP.Prelude

import ZP.Domain.Dynamic.Model.Common

import qualified Data.Map as Map
import qualified Data.Vector as V

data World = World
  { gWorldData :: V.Vector (V.Vector Char)
  , gWorldMap  :: Map.Map (Int, Int) Char     -- temporary; for dev convenience
  }
