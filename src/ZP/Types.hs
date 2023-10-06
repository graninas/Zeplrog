module ZP.Types where

import ZP.Prelude

import qualified Data.Map as Map

type Coords = (Int, Int)

newtype GridDimensions  = GridDimensions Coords
newtype GridCellSize    = GridCellSize Int
newtype BareCellSize    = BareCellSize Int
newtype BareCellHalf    = BareCellHalf Int
newtype BaseShift       = BaseShift Coords
newtype CellSpaceSize   = CellSpaceSize Int
newtype PlayerPosition  = PlayerPosition Coords

type RenderedLevel = Map.Map Coords Char
type Level = Map.Map Coords Char
