module Main where

import ZP.Prelude

import ZP.Types
import ZP.Gloss.Types
import ZP.Gloss.Conv
import ZP.Gloss.Render.Renderer
import ZP.Game.Logic
import ZP.Game.State

import qualified Data.Map as Map

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

defaultGlossWindowSize :: GlossWindowSize
defaultGlossWindowSize = GlossWindowSize (1200, 1200)

defaultGlossWindowPosition :: GlossWindowPosition
defaultGlossWindowPosition = GlossWindowPosition (500, 100)

defaultPlayerPosition :: PlayerPosition
defaultPlayerPosition = PlayerPosition (1, 1)

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
