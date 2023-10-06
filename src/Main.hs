module Main where

import qualified Data.Map as Map

import Graphics.Gloss


type Coords = (Int, Int)
type RenderedCell = Char

type RenderedLevel = Map.Map Coords RenderedCell


data GameState = GameState
  { plPos :: Coords
  , renderedLevel :: RenderedLevel
  }

data Room = RectRoom Coords Coords

firstRoom :: Room
firstRoom = RectRoom (0, 0) (20, 20)

-- TODO
renderLevel :: Room -> RenderedLevel
renderLevel _ = Map.fromList cells
  where
    cells = [ ((x, y), ' ') | x <- [1..20], y <- [1..20] ]

xBaseShift = (-100)
yBaseShift = (-100)

idxToGlossCell :: Int -> Float
idxToGlossCell x = fromIntegral $ x * 10

toGlossCell :: (Coords, Char) -> Picture
toGlossCell ((x, y), ' ') = Translate (idxToGlossCell x) (idxToGlossCell y) (Circle 10)

levelToGloss :: RenderedLevel -> Picture
levelToGloss lvl = Pictures $ map toGlossCell $ Map.toList lvl

initGame :: IO GameState
initGame = pure $ GameState (20, 20) (renderLevel firstRoom)

main :: IO ()
main = do

  -- gameSt <- initGame
  let renderedLevel = renderLevel firstRoom

  let glossLevel = levelToGloss renderedLevel

  -- display (InWindow "The Journey of Zeplrog" (600, 600) (10, 10)) white glossLevel


  -- let zeroCircle = Translate (-100.0) (100.0) $ Circle 10
  -- display (InWindow "The Journey of Zeplrog" (200, 200) (10, 10)) white zeroCircle
