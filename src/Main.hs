module Main where

import ZP.Prelude

import qualified Data.Map as Map

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- X is horizontal, grows to right
-- Y is vertical, grows to up
-- (-x, -y) is the left bottom corner
-- (0, 0) is in the center of the window

type Coords = (Int, Int)

newtype GridDimensions  = GridDimensions Coords
newtype GridCellSize    = GridCellSize Int
newtype BareCellSize    = BareCellSize Int
newtype BareCellHalf    = BareCellHalf Int
newtype BaseShift       = BaseShift Coords
newtype GlossBaseShift  = GlossBaseShift (Float, Float)
newtype GlossCoords     = GlossCoords (Float, Float)
newtype CellSpaceSize   = CellSpaceSize Int
newtype PlayerPosition  = PlayerPosition Coords
newtype GlossWindowSize = GlossWindowSize Coords
newtype GlossWindowPosition = GlossWindowPosition Coords

type RenderedLevel = Map.Map Coords Char
type Level = Map.Map Coords Char


data GameState = GameState
  { wndSizeVar        :: TVar GlossWindowSize
  , gridDimsVar       :: TVar GridDimensions
  , bareCellSizeVar   :: TVar BareCellSize
  , cellSpaceSizeVar  :: TVar CellSpaceSize
  , playerPosVar      :: TVar PlayerPosition
  , levelVar          :: TVar Level
  }

data Room = RectRoom Coords Coords

defaultGlossWindowSize :: GlossWindowSize
defaultGlossWindowSize = GlossWindowSize (1200, 1200)

defaultGlossWindowPosition :: GlossWindowPosition
defaultGlossWindowPosition = GlossWindowPosition (500, 100)

defaultPlayerPosition :: PlayerPosition
defaultPlayerPosition = PlayerPosition (500, 100)

-- | Number of cells in the grid
defaultGridDimensions :: GridDimensions
defaultGridDimensions = GridDimensions (26, 26)

defaultCellSize :: BareCellSize
defaultCellSize = BareCellSize 40

defaultCellSpaceSize :: CellSpaceSize
defaultCellSpaceSize = CellSpaceSize $ dcs `div` 10
  where
    (BareCellSize dcs) = defaultCellSize

initialLevel :: GridDimensions -> Level
initialLevel (GridDimensions (dimsX, dimsY)) = Map.fromList cells
  where
    cells = [ ((x, y), ' ') | x <- [1..dimsX], y <- [1..dimsY] ]


initGame
  :: GlossWindowSize
  -> GlossWindowPosition
  -> GridDimensions
  -> BareCellSize
  -> CellSpaceSize
  -> PlayerPosition
  -> Level
  -> IO (GameState, Display)
initGame
  (GlossWindowSize wndSize)
  (GlossWindowPosition wndPos)
  gridDims
  bareCellSize
  cellSpaceSize
  playerPos
  level = do
    let glossWindow = InWindow "The Journey of Zeplrog" wndSize wndPos
    st <- GameState
      <$> newTVarIO (GlossWindowSize wndSize)
      <*> newTVarIO gridDims
      <*> newTVarIO bareCellSize
      <*> newTVarIO cellSpaceSize
      <*> newTVarIO playerPos
      <*> newTVarIO level
    pure (st, glossWindow)

simpleGameSimulator :: Float -> GameState -> IO GameState
simpleGameSimulator _ st@(GameState {..}) = do
  atomically $ do
    PlayerPosition playerPos <- readTVar playerPosVar
    writeTVar playerPosVar $ PlayerPosition ( 1 + snd playerPos, fst playerPos )
  pure st

coordsToGlossCell :: GlossBaseShift -> GridCellSize -> Coords -> GlossCoords
coordsToGlossCell (GlossBaseShift (shiftX, shiftY)) (GridCellSize cellSize) (x, y)
  = GlossCoords (x', y')
  where
    x' = shiftX + fromIntegral (x * cellSize)
    y' = shiftY + fromIntegral (y * cellSize)

getBaseShift :: GlossWindowSize -> BaseShift
getBaseShift (GlossWindowSize (wX, wY)) = BaseShift (shiftX, shiftY)
  where
    shiftX = negate $ wX `div` 2
    shiftY = negate $ wY `div` 2

getGlossBaseShift :: GlossWindowSize -> GlossBaseShift
getGlossBaseShift gwnd = GlossBaseShift (fromIntegral shiftX, fromIntegral shiftY)
  where
    BaseShift (shiftX, shiftY) = getBaseShift gwnd

getBareCellHalf :: BareCellSize -> BareCellHalf
getBareCellHalf (BareCellSize s) = BareCellHalf $ s `div` 2

getGridCellSize :: BareCellSize -> CellSpaceSize -> GridCellSize
getGridCellSize (BareCellSize bareCellSize) (CellSpaceSize cellSpaceSize) =
  GridCellSize $ bareCellSize + cellSpaceSize

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
    glossCellShape = Circle $ fromIntegral bareCellHalf

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
    [ glossPlayerRenderer glossBaseShift gridCellSize bareCellHalf playerPos
    , glossLevelRenderer glossBaseShift gridCellSize bareCellHalf level
    ]


glossEvenHandler :: Event -> GameState -> IO GameState
glossEvenHandler (EventKey _ _ _ _)     st = pure st
glossEvenHandler (EventResize newSize)  st = pure st
glossEvenHandler (EventMotion mousePos) st = pure st



main :: IO ()
main = do

  (st, glossWindow) <- initGame
    defaultGlossWindowSize
    defaultGlossWindowPosition
    defaultGridDimensions
    defaultCellSize
    defaultCellSpaceSize
    defaultPlayerPosition
    (initialLevel defaultGridDimensions)

  playIO glossWindow black 2 st glossRenderer glossEvenHandler simpleGameSimulator
