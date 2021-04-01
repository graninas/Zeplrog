module Main where

import qualified Data.Map as Map

import Graphics.Gloss

-- X is horizontal, grows to right
-- Y is vertical, grows to up
-- (-x, -y) is the left bottom corner
-- (0, 0) is in the center of the window

type Coords = (Int, Int)
type RenderedCell = Char

type RenderedLevel = Map.Map Coords RenderedCell


data GameState = GameState
  { plPos :: Coords
  , renderedLevel :: RenderedLevel
  }

data Room = RectRoom Coords Coords

glossWindowSize :: Coords
glossWindowSize = (1200, 1200)

glossWindowPosition :: Coords
glossWindowPosition = (500, 100)

glossWindow :: Display
glossWindow = InWindow "The Journey of Zeplrog" glossWindowSize glossWindowPosition

xBaseShift, yBaseShift :: Int
xBaseShift = negate $ fst glossWindowSize `div` 2
yBaseShift = negate $ snd glossWindowSize `div` 2

xGlossBaseShift, yGlossBaseShift :: Float
xGlossBaseShift = fromIntegral xBaseShift
yGlossBaseShift = fromIntegral yBaseShift


firstRoom :: Room
firstRoom = RectRoom (0, 0) (20, 20)

cellSize :: Int
cellSize = 40

cellHalfSize :: Int
cellHalfSize = cellSize `div` 2

cellSpaceSize :: Int
cellSpaceSize = cellSize `div` 10

gridCellSize :: Int
gridCellSize = cellSize + cellSpaceSize

gridDimension :: Coords
gridDimension = (26, 26)

glossCellShape :: Picture
glossCellShape = Circle $ fromIntegral cellHalfSize

-- TODO
renderLevel :: Room -> RenderedLevel
renderLevel _ = Map.fromList cells
  where
    cells = [ ((x, y), ' ') | x <- [1..fst gridDimension], y <- [1..snd gridDimension] ]

idxXToGlossCell :: Int -> Float
idxXToGlossCell x = xGlossBaseShift + fromIntegral (x * gridCellSize)

idxYToGlossCell :: Int -> Float
idxYToGlossCell y = yGlossBaseShift + fromIntegral (y * gridCellSize)

toGlossCell :: (Coords, Char) -> Picture
toGlossCell ((x, y), ' ') = Translate (idxXToGlossCell x) (idxYToGlossCell y) glossCellShape

levelToGloss :: RenderedLevel -> Picture
levelToGloss lvl = Pictures $ map toGlossCell $ Map.toList lvl

initGame :: IO GameState
initGame = pure $ GameState (20, 20) (renderLevel firstRoom)

main :: IO ()
main = do

  -- gameSt <- initGame
  let renderedLevel = renderLevel firstRoom

  let glossLevel = levelToGloss renderedLevel

  display glossWindow black $ Color white glossLevel


  -- let zzCircle = Pictures
  --       [ Color white $ Translate 0 0 $ circleSolid 30
  --       , Color blue  $ Translate 0 0 $ circleSolid 5
  --       ]
  -- let nnCircle = Pictures
  --       [ Color white $ Translate xGlossBaseShift yGlossBaseShift $ circleSolid 30
  --       , Color blue  $ Translate xGlossBaseShift yGlossBaseShift $ circleSolid 5
  --       ]
  -- let znCircle = Pictures
  --       [ Color white $ Translate 0 yGlossBaseShift $ circleSolid 30
  --       , Color blue  $ Translate 0 yGlossBaseShift $ circleSolid 5
  --       ]
  -- display glossWindow black (Pictures [zzCircle, znCircle])
