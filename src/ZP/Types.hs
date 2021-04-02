module ZP.Types where

import ZP.Prelude

import qualified Data.Map as Map

type Coords = (Int, Int)

newtype CellIdxs = CellIdxs (Int, Int)
  deriving (Eq, Ord, Show)

newtype GridDimensions  = GridDimensions CellIdxs
newtype GridCellSize    = GridCellSize Int
newtype BareCellSize    = BareCellSize Int
newtype BareCellHalf    = BareCellHalf Int
newtype BaseShift       = BaseShift Coords
newtype CellSpaceSize   = CellSpaceSize Int
newtype PlayerPosition  = PlayerPosition CellIdxs

type Level = Map.Map CellIdxs Char
