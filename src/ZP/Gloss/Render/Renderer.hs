module ZP.Gloss.Render.Renderer where

import ZP.Prelude

import ZP.Types
import ZP.Gloss.Types
import ZP.Gloss.Conv
import ZP.Game.State

import Graphics.Gloss

import qualified Data.Map as Map

glossPlayerRenderer
  :: GlossBaseShift
  -> GridCellSize
  -> BareCellHalf
  -> PlayerPosition
  -> Picture
glossPlayerRenderer glossBaseShift gridCellSize (BareCellHalf bareCellHalf) (PlayerPosition playerPos) =
  Translate glossCellX glossCellY glossPlayerShape
  where
    glossPlayerShape :: Picture
    glossPlayerShape = Color red $ circleSolid $ fromIntegral bareCellHalf
    (GlossCoords (glossCellX, glossCellY)) = coordsToGlossCell glossBaseShift gridCellSize playerPos


glossLevelRenderer
  :: GlossBaseShift
  -> GridCellSize
  -> BareCellHalf
  -> Level
  -> Picture
glossLevelRenderer glossBaseShift gridCellSize (BareCellHalf bareCellHalf) level =
  Pictures $ map toGlossCell $ Map.toList level
  where
    glossCellShape :: Picture
    glossCellShape = Color white $ Circle $ fromIntegral bareCellHalf

    toGlossCell :: (Coords, Char) -> Picture
    toGlossCell (cellPos, ' ') = Translate shiftX shiftY glossCellShape
      where
        (GlossCoords (shiftX, shiftY)) = coordsToGlossCell glossBaseShift gridCellSize cellPos



glossRenderer :: GameState -> IO Picture
glossRenderer (GameState {..}) = do
  (wndSize, bareCellSize, cellSpaceSize, playerPos, level) <- atomically $ do
    wndSize       <- readTVar wndSizeVar
    bareCellSize  <- readTVar bareCellSizeVar
    cellSpaceSize <- readTVar cellSpaceSizeVar
    playerPos     <- readTVar playerPosVar
    level         <- readTVar levelVar
    pure (wndSize, bareCellSize, cellSpaceSize, playerPos, level)

  let gridCellSize   = getGridCellSize bareCellSize cellSpaceSize
  let glossBaseShift = getGlossBaseShift wndSize
  let bareCellHalf   = getBareCellHalf bareCellSize

  pure $ Pictures
    [ glossLevelRenderer glossBaseShift gridCellSize bareCellHalf level
    , glossPlayerRenderer glossBaseShift gridCellSize bareCellHalf playerPos
    ]
