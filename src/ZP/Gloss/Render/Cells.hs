module ZP.Gloss.Render.Cells where

import ZP.Prelude

import ZP.Types
import ZP.Gloss.Types

import Graphics.Gloss

import qualified Data.Map as Map


emptyCell :: Picture
emptyCell = Color white $ circleSolid 5


clearFloor :: Picture
clearFloor = Color (greyN 0.8) $ text "."

pillar :: Picture
pillar = Color (light green) $ text "I"

door :: Picture
door = Color (dark yellow) $ text "+"

wall :: String -> Picture
wall w = Color white $ text w


unknown :: String -> Picture
unknown w = Color red $ text $ "?" <> w
