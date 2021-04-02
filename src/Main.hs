module Main where

import ZP.Prelude

import ZP.Types
import ZP.Gloss.Types
import ZP.Gloss.Conv
import ZP.Gloss.Render.Renderer
import ZP.Gloss.Render.Shapes
import ZP.Game.Types
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

-- | Number of cells in the grid
defaultGridDimensions :: GridDimensions
defaultGridDimensions = GridDimensions $ CellIdxs (26, 26)

defaultBareCellSize :: BareCellSize
defaultBareCellSize = BareCellSize 40

defaultCellSpaceSize :: CellSpaceSize
defaultCellSpaceSize = CellSpaceSize $ dcs `div` 10
  where
    (BareCellSize dcs) = defaultBareCellSize

defaultDbgOptions :: DebugOptions
defaultDbgOptions = DebugOptions True white True (dark green) False "Hi, this is debug!" (dark green)


initialPlayerActorState :: BareCellSize -> IO ActorState
initialPlayerActorState bareCellSize = ActorState
    <$> newTVarIO FindExit            -- goal (TODO)
    <*> newTVarIO FollowingPath       -- current activity

    <*> newTVarIO (CellIdxs (3, 3))   -- current pos
    <*> newTVarIO                     -- current path
      [ CellIdxs (4,3)
      , CellIdxs (4,4)
      , CellIdxs (5,4)
      , CellIdxs (6,4)
      , CellIdxs (7,4)
      , CellIdxs (8,4)
      , CellIdxs (8,5)
      ]

    <*> newTVarIO (playerActorShape bareCellSize)
    <*> newTVarIO (pathPointShape bareCellSize)
    <*> newTVarIO (PathIsBlinking 2)          -- TODO: not hardcoded blink period
    <*> pure (pathPointShape bareCellSize)

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
  -> ActorState
  -> Level
  -> DebugOptions
  -> IO (GameState, Display)
initGame
  (GlossWindowSize wndSize)
  (GlossWindowPosition wndPos)
  gridDims
  bareCellSize
  cellSpaceSize
  playerActor
  level
  dbgOpts = do
    let glossWindow = InWindow "The Journey of Zeplrog" wndSize wndPos
    st <- GameState
      <$> newTVarIO (GlossWindowSize wndSize)
      <*> newTVarIO gridDims
      <*> newTVarIO bareCellSize
      <*> newTVarIO cellSpaceSize
      <*> (pure playerActor)
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

  playerActor <- initialPlayerActorState defaultBareCellSize

  (st, glossWindow) <- initGame
    defaultGlossWindowSize
    defaultGlossWindowPosition
    defaultGridDimensions
    defaultBareCellSize
    defaultCellSpaceSize
    playerActor
    lvl
    defaultDbgOptions

  playIO glossWindow black 2 st glossRenderer glossEvenHandler simpleGameSimulator
