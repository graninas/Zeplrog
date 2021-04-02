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
  , currentShape          :: Picture

  , currentPathPointShape :: Picture
  }

data AnimatedObjectSlice = AnimatedPathSlice ActorPath (Maybe PathDisplay)

type AnimatedObjectsSlice = [AnimatedObjectSlice]

readPlayerActor :: ActorState -> STM ActorStateSlice
readPlayerActor (ActorState {..}) =
  ActorStateSlice
    <$> readTVar goalVar
    <*> readTVar currentActivityVar

    <*> readTVar currentPosVar
    <*> readTVar currentShapeVar

    <*> readTVar currentPathPointShapeVar

readAnimatedObjects :: TVar AnimatedObjects -> STM AnimatedObjectsSlice
readAnimatedObjects objectsVar = do
  objMap <- readTVar objectsVar
  mapM toAnimObjectSlice $ Map.toList objMap
  where
    toAnimObjectSlice :: (ObjectId, AnimatedObject) -> STM AnimatedObjectSlice
    toAnimObjectSlice (_, AnimatedPath actPathVar) = do
      ActivePath _ path mbDisp <- readTVar actPathVar
      pure $ AnimatedPathSlice path mbDisp

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
        Idling n        -> text $ "idling: " <> show n
        Observing _ n _ -> text $ "observing: " <> show n
        FollowingPath _ -> text $ "following the path"
  in Translate x y $ Scale scaleFactor scaleFactor $ Color red actPicture


renderAnimatedObjects :: RenderOptions -> AnimatedObjectsSlice -> Picture
renderAnimatedObjects ro objs = Pictures $ map (renderAnimatedObjectSlice ro) objs

renderAnimatedObjectSlice :: RenderOptions -> AnimatedObjectSlice -> Picture
renderAnimatedObjectSlice ro (AnimatedPathSlice path mbDisp) = renderPath ro path mbDisp

renderPath :: RenderOptions -> ActorPath -> Maybe PathDisplay -> Picture
renderPath _ _ Nothing = blank
renderPath (RenderOptions {..}) path (Just (PathIsBlinking pic n)) =
  if n >= pathBlinkingHalfPeriod
    then Pictures $ map toPathPoint path
    else blank
  where
    toPathPoint :: CellIdxs -> Picture
    toPathPoint pos = Translate glossCellX glossCellY pic
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
  (wndSize, bareCellSize, cellSpaceSize, playerActorSlice, level, animObjsSlice, dbgOpts) <- atomically $ do
    wndSize       <- readTVar wndSizeVar
    bareCellSize  <- readTVar bareCellSizeVar
    cellSpaceSize <- readTVar cellSpaceSizeVar
    playerActorSlice   <- readPlayerActor playerActorState
    level         <- readTVar levelVar
    animObjsSlice <- readAnimatedObjects animatedObjectsVar
    dbgOpts       <- readTVar debugOptionsVar
    pure (wndSize, bareCellSize, cellSpaceSize, playerActorSlice, level, animObjsSlice, dbgOpts)

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
    [ renderLevel renderOptions level
    , renderActor renderOptions playerActorSlice
    , renderAnimatedObjects renderOptions animObjsSlice
    , renderActorActivity renderOptions playerActorSlice
    , renderDebugText renderOptions
    ]
