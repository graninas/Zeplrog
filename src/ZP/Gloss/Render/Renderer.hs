module ZP.Gloss.Render.Renderer where

import ZP.Prelude

import ZP.Types
import ZP.Gloss.Types
import ZP.Gloss.Conv
import ZP.Gloss.Render.Shapes
import ZP.Game.Types
import ZP.Game.State
import ZP.Game.Debug
import ZP.Hardcode

import Graphics.Gloss

import qualified Data.Map as Map

data RenderOptions = RenderOptions
  { debugOptions   :: DebugOptions
  , gridCellSize   :: GridCellSize
  , bareCellSize   :: BareCellSize
  , glossBaseShift :: GlossBaseShift
  , glossBareCellSize :: GlossBareCellSize

  , glossTextScale :: GlossTextScale

  , glossInfoWindowPos :: GlossCoords
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

renderActorActivity :: RenderOptions -> ActorStateSlice -> Picture
renderActorActivity (RenderOptions {..}) (ActorStateSlice {..}) =
  let
    GlossCoords (x, y) = glossInfoWindowPos
    GlossTextScale scaleFactor = glossTextScale
    actPicture = case currentActivity of
        Idling n          -> text $ "idling: " <> show n
        Observing n mbRes -> text $ "observing: " <> show n
        FollowingPath     -> text $ "following the path"
  in Translate x y $ Scale scaleFactor scaleFactor $ Color red actPicture

renderActorPath :: RenderOptions -> ActorStateSlice -> Picture
renderActorPath (RenderOptions {..}) (ActorStateSlice {..}) =
    case currentPathDisplay of
      PathIsBlinking n | n >= pathBlinkingHalfPeriod -> Pictures $ map toActorPathPoint currentPath
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

  -- TODO: remove hardcode
  let glossTextScale         = GlossTextScale 0.2
  let infoWindowPos          = CellIdxs (17, 9)

  let glossInfoWindowPos = coordsToGlossCell glossBaseShift gridCellSize infoWindowPos


  let renderOptions = RenderOptions
        dbgOpts
        gridCellSize
        bareCellSize
        glossBaseShift
        glossBareCellSize
        glossTextScale
        glossInfoWindowPos

  pure $ Pictures
    [ renderLevel      renderOptions level
    , renderActorPath  renderOptions playerActor
    , renderActor      renderOptions playerActor
    , renderActorActivity renderOptions playerActor
    , renderDebugText  renderOptions
    ]
