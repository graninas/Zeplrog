module ZP.Game.Logic where

import ZP.Prelude

import ZP.Types
import ZP.Game.State

import qualified Data.Map as Map


simpleGameSimulator :: Float -> GameState -> IO GameState
simpleGameSimulator _ st@(GameState {..}) = do
  atomically $ do
    PlayerPosition playerPos <- readTVar playerPosVar
    writeTVar playerPosVar $ PlayerPosition ( 1 + snd playerPos, fst playerPos )
  pure st
