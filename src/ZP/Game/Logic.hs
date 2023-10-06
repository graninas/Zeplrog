module ZP.Game.Logic where

import ZP.Prelude

import ZP.Types
import ZP.Game.State
import ZP.Game.Types

import qualified Data.Map as Map


simpleGameSimulator :: Float -> GameState -> IO GameState
simpleGameSimulator _ st@(GameState {..}) = do
  let ActorState {..} = playerActorState
  atomically $ do
    dispVar <- readTVar currentPathDisplayVar
    case dispVar of
      PathIsInvisible           -> pure ()
      PathIsBlinking n | n > 0  -> writeTVar currentPathDisplayVar $ PathIsBlinking $ n - 1   -- TODO: not hardcoded blink period
      PathIsBlinking n | n == 0 -> writeTVar currentPathDisplayVar $ PathIsBlinking $ 3       -- TODO: not hardcoded blink period
  pure st
