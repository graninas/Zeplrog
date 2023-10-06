module ZP.Game.Logic where

import ZP.Prelude

import ZP.Types
import ZP.Game.State
import ZP.Game.Types

import qualified Data.Map as Map

animateActorPath :: ActorState -> STM ()
animateActorPath ActorState {..} = do
  dispVar <- readTVar currentPathDisplayVar
  case dispVar of
    PathIsInvisible           -> pure ()
    PathIsBlinking n | n > 0  -> writeTVar currentPathDisplayVar $ PathIsBlinking $ n - 1   -- TODO: not hardcoded blink period
    PathIsBlinking n | n == 0 -> writeTVar currentPathDisplayVar $ PathIsBlinking $ 3       -- TODO: not hardcoded blink period


moveActorByPath :: ActorState -> STM ()
moveActorByPath ActorState {..} = do
  path <- readTVar currentPathVar
  case path of
    [] -> pure ()
    (p:ps) -> do
      writeTVar currentPosVar p
      writeTVar currentPathVar ps



simpleGameSimulator :: Float -> GameState -> IO GameState
simpleGameSimulator _ st@(GameState {..}) = do
  atomically $ do
    animateActorPath playerActorState
    moveActorByPath playerActorState
  pure st
