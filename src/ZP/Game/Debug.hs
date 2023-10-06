module ZP.Game.Debug where

import ZP.Prelude

import ZP.Types
import ZP.Gloss.Types

import Graphics.Gloss

import qualified Data.Map as Map

data DebugOptions = DebugOptions
  { dbgShowCellBoxes   :: Bool
  , dbgCellBoxColor    :: Color
  , dbgShowGrid        :: Bool
  , dbgGridColor       :: Color
  
  , dbgShowDebugText   :: Bool
  , dbgDebugText       :: String
  , dbgDebugTextColor  :: Color
  }
