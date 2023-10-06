module ZP.Gloss.Render.Renderer where

import ZP.Prelude

import ZP.Types
import ZP.Gloss.Types
import ZP.Gloss.Conv
import ZP.Gloss.Render.Shapes
import ZP.Game.Types
import ZP.Game.State
import ZP.Game.Debug

import Graphics.Gloss

import qualified Data.Map as Map

data ActorStateSlice = ActorStateSlice
  { goalVar               :: Goal
  , currentPos            :: CellIdxs
  , currentPath           :: ActorPath
  , currentShape          :: Picture

  , currentPathPointShape :: Picture
  , currentPathDisplay    :: PathDisplay
  , staticShape           :: Picture
  }

readPlayerActor :: ActorState -> STM ActorStateSlice
readPlayerActor (ActorState {..}) =
  ActorStateSlice
    <$> readTVar goalVar
    <*> readTVar currentPosVar
    <*> readTVar currentPathVar
    <*> readTVar currentShapeVar
    <*> readTVar currentPathPointShapeVar
    <*> readTVar currentPathDisplayVar
    <*> pure staticShape


glossActorRenderer
  :: DebugOptions
  -> GlossBaseShift
  -> GridCellSize
  -> ActorStateSlice
  -> Picture
glossActorRenderer
  dbgOpts
  glossBaseShift
  gridCellSize
  (ActorStateSlice {..}) =
  Translate glossCellX glossCellY currentShape
  where
    (GlossCoords (glossCellX, glossCellY)) = coordsToGlossCell glossBaseShift gridCellSize currentPos

glossActorPathRenderer
  :: DebugOptions
  -> GlossBaseShift
  -> GridCellSize
  -> ActorStateSlice
  -> Picture
glossActorPathRenderer
  dbgOpts
  glossBaseShift
  gridCellSize
  (ActorStateSlice {..}) =
    -- TODO: not hardcoded blink period 
    case currentPathDisplay of
      PathIsBlinking n | n >= 2 -> Pictures $ map toActorPathPoint currentPath
      _ -> blank
  where
    toActorPathPoint :: CellIdxs -> Picture
    toActorPathPoint pos = Translate glossCellX glossCellY currentPathPointShape
      where
        (GlossCoords (glossCellX, glossCellY)) = coordsToGlossCell glossBaseShift gridCellSize pos

glossLevelRenderer
  :: DebugOptions
  -> GlossBaseShift
  -> GridCellSize
  -> BareCellSize
  -> Level
  -> Picture
glossLevelRenderer
  (DebugOptions {..})
  glossBaseShift
  gridCellSize
  (BareCellSize bareCellSize)
  level =
  Pictures $ map (toGlossCell'' . withGlossCoords) $ Map.toList level
  where
    glossBareCellSize :: GlossBareCellSize
    glossBareCellSize = GlossBareCellSize $ fromIntegral bareCellSize

    withGlossCoords :: (CellIdxs, Char) -> (GlossCoords, Char)
    withGlossCoords (cellPos, ch) =
      (coordsToGlossCell glossBaseShift gridCellSize cellPos, ch)

    toGlossCell'' :: (GlossCoords, Char) -> Picture
    toGlossCell'' (GlossCoords (shiftX, shiftY), ch) =
      if showCellBoxes
      then Pictures [ Translate shiftX shiftY $ toGlossCell glossBareCellSize ch
                    , Translate shiftX shiftY $ cellBox glossBareCellSize cellBoxColor
                    ]
      else Translate shiftX shiftY $ toGlossCell glossBareCellSize ch

toGlossCell :: GlossBareCellSize -> Char -> Picture
toGlossCell _ '#' = emptyCell
toGlossCell _ '.' = clearFloor
toGlossCell cs 'I' = pillar cs
toGlossCell cs '+' = door cs
toGlossCell cs '─' = hWall cs
toGlossCell cs '│' = vWall cs
toGlossCell cs '┘' = brCorner cs
toGlossCell cs '└' = blCorner cs
toGlossCell cs '┌' = ulCorner cs
toGlossCell cs '┐' = urCorner cs
toGlossCell cs '├' = vWallRJoint cs
toGlossCell cs '┤' = vWallLJoint cs
toGlossCell cs ch  = unknown cs [ch]

glossDebugTextRenderer :: DebugOptions -> GlossBaseShift -> GridCellSize -> Picture
glossDebugTextRenderer
  (DebugOptions {..})
  glossBaseShift
  gridCellSize
  = if showDebugText
    then Pictures
      [ Translate x y $ Color white $ circleSolid 5

      -- , Translate x y $ Scale 0.1 0.1 $ Color (dark $ dark $ dark $ debugTextColor) $ text debugText
      -- , Translate x y $ Scale 0.2 0.2 $ Color (dark $ dark $ debugTextColor) $ text debugText
      , Translate x y $ Scale 0.3 0.3 $ Color (dark $ debugTextColor) $ text debugText
      -- , Translate x y $ Scale 0.4 0.4 $ Color (debugTextColor) $ text debugText

      -- , Scale 0.1 0.1 $ Translate x y $ Color (dark $ dark $ dark $ debugTextColor) $ text debugText
      -- , Scale 0.2 0.2 $ Translate x y $ Color (dark $ dark $ debugTextColor) $ text debugText
      -- , Scale 0.3 0.3 $ Translate x y $ Color (dark $ debugTextColor) $ text debugText
      -- , Scale 0.4 0.4 $ Translate x y $ Color (debugTextColor) $ text debugText
      ]
    else blank
  where
    idxs = CellIdxs (17, 10)
    GlossCoords (x, y) = coordsToGlossCell glossBaseShift gridCellSize idxs

glossRenderer :: GameState -> IO Picture
glossRenderer (GameState {..}) = do
  (wndSize, bareCellSize, cellSpaceSize, playerActor, level, dbgOpts) <- atomically $ do
    wndSize       <- readTVar wndSizeVar
    bareCellSize  <- readTVar bareCellSizeVar
    cellSpaceSize <- readTVar cellSpaceSizeVar
    playerActor   <- readPlayerActor playerActorState
    level         <- readTVar levelVar
    dbgOpts       <- readTVar debugOptionsVar
    pure (wndSize, bareCellSize, cellSpaceSize, playerActor, level, dbgOpts)

  let gridCellSize      = getGridCellSize bareCellSize cellSpaceSize
  let glossBaseShift    = getGlossBaseShift wndSize
  let bareCellHalf      = getBareCellHalf bareCellSize
  let glossGridCellSize = getGlossGridCellSize gridCellSize

  pure $ Pictures
    [ glossLevelRenderer     dbgOpts glossBaseShift gridCellSize bareCellSize level
    , glossActorPathRenderer dbgOpts glossBaseShift gridCellSize playerActor
    , glossActorRenderer     dbgOpts glossBaseShift gridCellSize playerActor
    , glossDebugTextRenderer dbgOpts glossBaseShift gridCellSize
    ]