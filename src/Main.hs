module Main where

import ZP.Prelude

import ZP.Types
import ZP.Gloss.Types
import ZP.Gloss.Conv
import ZP.Gloss.Render.Renderer
import ZP.Game.Logic
import ZP.Game.State
import ZP.Game.Debug

import qualified Data.Map as Map
import qualified Data.Text as T

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

defaultGlossWindowSize :: GlossWindowSize
defaultGlossWindowSize = GlossWindowSize (1200, 1200)

defaultGlossWindowPosition :: GlossWindowPosition
defaultGlossWindowPosition = GlossWindowPosition (500, 100)

defaultPlayerPosition :: PlayerPosition
defaultPlayerPosition = PlayerPosition $ CellIdxs (3, 3)

-- | Number of cells in the grid
defaultGridDimensions :: GridDimensions
defaultGridDimensions = GridDimensions $ CellIdxs (26, 26)

defaultCellSize :: BareCellSize
defaultCellSize = BareCellSize 40

defaultCellSpaceSize :: CellSpaceSize
defaultCellSpaceSize = CellSpaceSize $ dcs `div` 10
  where
    (BareCellSize dcs) = defaultCellSize

defaultDbgOptions :: DebugOptions
defaultDbgOptions = DebugOptions True white True (dark green) True "Hi, this is debug!" (dark green)

initialLevel :: GridDimensions -> Level
initialLevel (GridDimensions (CellIdxs (dimsX, dimsY))) = Map.fromList cells
  where
    cells = [ (CellIdxs (x, y), ' ') | x <- [1..dimsX], y <- [1..dimsY] ]


initGame
  :: GlossWindowSize
  -> GlossWindowPosition
  -> GridDimensions
  -> BareCellSize
  -> CellSpaceSize
  -> PlayerPosition
  -> Level
  -> DebugOptions
  -> IO (GameState, Display)
initGame
  (GlossWindowSize wndSize)
  (GlossWindowPosition wndPos)
  gridDims
  bareCellSize
  cellSpaceSize
  playerPos
  level
  dbgOpts = do
    let glossWindow = InWindow "The Journey of Zeplrog" wndSize wndPos
    st <- GameState
      <$> newTVarIO (GlossWindowSize wndSize)
      <*> newTVarIO gridDims
      <*> newTVarIO bareCellSize
      <*> newTVarIO cellSpaceSize
      <*> newTVarIO playerPos
      <*> newTVarIO level
      <*> newTVarIO dbgOpts
    pure (st, glossWindow)


glossEvenHandler :: Event -> GameState -> IO GameState
glossEvenHandler (EventKey _ _ _ _)     st = pure st
glossEvenHandler (EventResize newSize)  st = pure st
glossEvenHandler (EventMotion mousePos) st = pure st


loadLevel :: String -> IO Level
loadLevel lvlFileName = do
  l1 :: [String] <- (reverse . map T.unpack . lines) <$> (readFile $ "./data/" <> lvlFileName)
  let l2 :: [(Int, String)] = zip [1..] l1
  let l3 :: [ (CellIdxs, Char) ] = join $ map zipRow l2
  pure $ Map.fromList l3
  where
    zipRow :: (Int, String) -> [ (CellIdxs, Char) ]
    zipRow (y, str) = [ (CellIdxs (x, y), ch) | (x, ch) <- zip [1..] str ]


main :: IO ()
main = do

  lvl <- loadLevel "lvl.txt"

  (st, glossWindow) <- initGame
    defaultGlossWindowSize
    defaultGlossWindowPosition
    defaultGridDimensions
    defaultCellSize
    defaultCellSpaceSize
    defaultPlayerPosition
    lvl
    defaultDbgOptions

  playIO glossWindow black 2 st glossRenderer glossEvenHandler simpleGameSimulator
