module ZP.Game.Debug where

import ZP.Prelude

import ZP.Types
import ZP.Gloss.Types

import Graphics.Gloss

import qualified Data.Map as Map

data DebugOptions = DebugOptions
  { showCellBoxes :: Bool
  , cellBoxColor :: Color
  , showGrid :: Bool
  , gridColor :: Color
  , showDebugText :: Bool
  , debugText :: String
  , debugTextColor :: Color
  }
