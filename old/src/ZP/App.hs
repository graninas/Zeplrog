module ZP.App where

import ZP.Prelude

import ZP.Types
import ZP.Game.Types

import qualified Data.Map as Map
import qualified Data.Text as T


loadLevel :: String -> IO Level
loadLevel lvlFileName = do
  l1 :: [String] <- (reverse . map T.unpack . lines) <$> (readFile lvlFileName)
  let l2 :: [(Int, String)] = zip [1..] l1
  let l3 :: [ (CellIdxs, Char) ] = join $ map zipRow l2
  pure $ Map.fromList l3
  where
    zipRow :: (Int, String) -> [ (CellIdxs, Char) ]
    zipRow (y, str) = [ (CellIdxs (x, y), ch) | (x, ch) <- zip [1..] str ]
