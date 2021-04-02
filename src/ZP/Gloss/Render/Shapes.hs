module ZP.Gloss.Render.Shapes where

import ZP.Prelude

import ZP.Types
import ZP.Gloss.Types

import Graphics.Gloss

import qualified Data.Map as Map

text' :: String -> Picture
text' str = Scale (0.13) (0.13) $ text str

emptyCell :: Picture
emptyCell = blank

clearFloor :: Picture
clearFloor = Color (greyN 0.8) $ circleSolid 2

pillar :: GlossBareCellSize -> Picture
pillar (GlossBareCellSize s) = Color green $ rectangleSolid (s / 3) s

door :: GlossBareCellSize -> Picture
door (GlossBareCellSize s) = Color (dark yellow) $ Pictures
  [ rectangleSolid (s / 3) s
  , rectangleSolid s (s / 3)
  ]

unknown :: GlossBareCellSize -> String -> Picture
unknown (GlossBareCellSize s) w = Color red $ Pictures
  [ rectangleWire s s
  , line [(nsDiv2, nsDiv2), (sDiv2, sDiv2)]
  , line [(nsDiv2, sDiv2), (sDiv2, nsDiv2)]
  ]
  where
    sDiv2 = s / 2
    nsDiv2 = negate sDiv2


cellBox :: GlossBareCellSize -> Color -> Picture
cellBox (GlossBareCellSize s) col = Color col $ rectangleWire s s

wallColor :: Color
wallColor = greyN 0.7

wallBrick :: GlossBareCellSize -> Picture
wallBrick (GlossBareCellSize s) = Color wallColor $ rectangleSolid s s

hWall :: GlossBareCellSize -> Picture
hWall = wallBrick
vWall :: GlossBareCellSize -> Picture
vWall = wallBrick
brCorner :: GlossBareCellSize -> Picture
brCorner = wallBrick
blCorner :: GlossBareCellSize -> Picture
blCorner = wallBrick
ulCorner :: GlossBareCellSize -> Picture
ulCorner = wallBrick
urCorner :: GlossBareCellSize -> Picture
urCorner = wallBrick
vWallRJoint :: GlossBareCellSize -> Picture
vWallRJoint = wallBrick
vWallLJoint :: GlossBareCellSize -> Picture
vWallLJoint = wallBrick


playerActorShape :: BareCellSize -> Picture
playerActorShape (BareCellSize bareCellSize) =
  Color red $ circleSolid $ fromIntegral (bareCellSize `div` 2)

pathPointShape :: BareCellSize -> Picture
pathPointShape (BareCellSize bareCellSize) =
  Color (dark green) $ circleSolid $ (fromIntegral bareCellSize / 4)
