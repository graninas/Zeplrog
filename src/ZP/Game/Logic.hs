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


evaluateActorActivity :: ActorState -> STM ()
evaluateActorActivity actorSt@(ActorState {..}) = do
  activity <- readTVar currentActivityVar
  curPath  <- readTVar currentPathVar
  case activity of
    Idling n | n > 0     -> writeTVar currentActivityVar $ Idling $ n - 1
    Idling n | n == 0    -> writeTVar currentActivityVar $ Observing 10 Nothing       -- TODO: remove hardcoded observing
    Observing n mbRes | n > 0  -> do

      -- TODO: observe

      writeTVar currentActivityVar $ Observing (n - 1) mbRes

    Observing n mbRes | n == 0 -> do
      case mbRes of
        Nothing -> writeTVar currentActivityVar $ Idling 10       -- TODO: remove hardcoded idling
        Just (PathFound path) -> do
          writeTVar currentPathVar path
          writeTVar currentActivityVar FollowingPath
    FollowingPath -> do
      animateActorPath actorSt
      case curPath of
        [] -> writeTVar currentActivityVar $ Idling 10
        _  -> moveActorByPath actorSt



simpleGameSimulator :: Float -> GameState -> IO GameState
simpleGameSimulator _ st@(GameState {..}) = do
  atomically $ do
    evaluateActorActivity playerActorState
  pure st
