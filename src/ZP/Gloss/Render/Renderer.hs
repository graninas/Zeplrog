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

data RenderOptions = RenderOptions
  { debugOptions   :: DebugOptions
  , gridCellSize   :: GridCellSize
  , bareCellSize   :: BareCellSize
  , glossBaseShift :: GlossBaseShift
  , glossBareCellSize :: GlossBareCellSize
  }

data ActorStateSlice = ActorStateSlice
  { goalVar               :: ActorGoal
  , currentActivity       :: ActorActivity

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
    <*> readTVar currentActivityVar

    <*> readTVar currentPosVar
    <*> readTVar currentPathVar
    <*> readTVar currentShapeVar

    <*> readTVar currentPathPointShapeVar
    <*> readTVar currentPathDisplayVar
    <*> pure staticShape


renderActor :: RenderOptions -> ActorStateSlice -> Picture
renderActor (RenderOptions {..}) (ActorStateSlice {..}) =
  Translate glossCellX glossCellY currentShape
  where
    (GlossCoords (glossCellX, glossCellY)) = coordsToGlossCell glossBaseShift gridCellSize currentPos

renderActorPath :: RenderOptions -> ActorStateSlice -> Picture
renderActorPath (RenderOptions {..}) (ActorStateSlice {..}) =
    -- TODO: not hardcoded blink period
    case currentPathDisplay of
      PathIsBlinking n | n >= 2 -> Pictures $ map toActorPathPoint currentPath
      _ -> blank
  where
    toActorPathPoint :: CellIdxs -> Picture
    toActorPathPoint pos = Translate glossCellX glossCellY currentPathPointShape
      where
        (GlossCoords (glossCellX, glossCellY)) = coordsToGlossCell glossBaseShift gridCellSize pos

renderLevel :: RenderOptions -> Level -> Picture
renderLevel (RenderOptions {..}) level =
  Pictures $ map (toGlossCell'' . withGlossCoords) $ Map.toList level
  where
    withGlossCoords :: (CellIdxs, Char) -> (GlossCoords, Char)
    withGlossCoords (cellPos, ch) =
      (coordsToGlossCell glossBaseShift gridCellSize cellPos, ch)

    toGlossCell'' :: (GlossCoords, Char) -> Picture
    toGlossCell'' (GlossCoords (shiftX, shiftY), ch) =
      if dbgShowCellBoxes debugOptions
      then Pictures [ Translate shiftX shiftY $ toGlossCell glossBareCellSize ch
                    , Translate shiftX shiftY $ cellBox glossBareCellSize $ dbgCellBoxColor debugOptions
                    ]
      else Translate shiftX shiftY $ toGlossCell glossBareCellSize ch

renderDebugText :: RenderOptions -> Picture
renderDebugText (RenderOptions {..}) =
  let DebugOptions {..} = debugOptions
  in if dbgShowDebugText
    then Pictures
      [ Translate x y $ Color white $ circleSolid 5
      , Translate x y $ Scale 0.3 0.3 $ Color (dark dbgDebugTextColor) $ text dbgDebugText
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
  let glossBareCellSize = getGlossBareCellSize bareCellSize

  let renderOptions = RenderOptions dbgOpts gridCellSize bareCellSize glossBaseShift glossBareCellSize

  pure $ Pictures
    [ renderLevel      renderOptions level
    , renderActorPath  renderOptions playerActor
    , renderActor      renderOptions playerActor
    , renderDebugText  renderOptions
    ]
