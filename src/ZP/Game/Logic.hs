module ZP.Game.Logic where

import ZP.Prelude

import ZP.Types
import ZP.Game.State
import ZP.Game.Types
import ZP.Hardcode

import qualified Data.Map as Map

getObjectId :: TVar ObjectId -> STM ObjectId
getObjectId idCounterVar = do
  pId <- readTVar idCounterVar
  modifyTVar' idCounterVar (\(ObjectId oId) -> ObjectId $ oId + 1)
  pure pId

createActivePath :: TVar ObjectId -> ActorPath -> STM (ObjectId, TVar ActivePath)
createActivePath idCounterVar path = do
  pId <- getObjectId idCounterVar
  var <- newTVar $ ActivePath pId path (Just initPathBlinkingDisplay)       -- TODO: remove hardcode
  pure (pId, var)

addAnimatedObject :: TVar AnimatedObjects -> ObjectId -> AnimatedObject -> STM ()
addAnimatedObject animatedObjectsVar objId animObj =
  modifyTVar' animatedObjectsVar $ Map.insert objId animObj

removeAnimatedObject :: TVar AnimatedObjects -> ObjectId -> STM ()
removeAnimatedObject animatedObjectsVar objId =
  modifyTVar' animatedObjectsVar $ Map.delete objId

animate :: TVar AnimatedObjects -> STM ()
animate objectsVar = do
  objects <- readTVar objectsVar
  mapM_ animateObject $ Map.toList objects

animateObject :: (ObjectId, AnimatedObject) -> STM ()
animateObject (_, AnimatedPath actPathVar) = animatePath actPathVar

animatePath :: TVar ActivePath -> STM ()
animatePath actPathVar = do
  actPath <- readTVar actPathVar
  case actPath of
    (ActivePath _ _ Nothing) -> pure ()

    (ActivePath objId path (Just (PathIsBlinking pic n))) | n > 0 ->
      writeTVar actPathVar (ActivePath objId path (Just (PathIsBlinking pic $ n - 1)))

    (ActivePath objId path (Just (PathIsBlinking pic n))) | n <= 0 ->
      writeTVar actPathVar (ActivePath objId path (Just (PathIsBlinking pic pathBlinkingPeriod)))

moveActor :: ActorState -> CellIdxs -> STM ()
moveActor ActorState {..} newPos = writeTVar currentPosVar newPos

evaluateActorActivity :: GameState -> ActorState -> STM ()
evaluateActorActivity (GameState {..}) actorSt@(ActorState {..}) = do
  activity <- readTVar currentActivityVar
  case activity of
    Idling n | n > 0     -> writeTVar currentActivityVar $ Idling $ n - 1
    Idling n | n <= 0    -> writeTVar currentActivityVar $ Observing False observingPeriod Nothing


    -- TODO: remove demo hardcode!
    Observing True n _ | n == 4  -> do
      (pId, pathVar) <- createActivePath objectIdCounterVar demoPath
      writeTVar currentActivityVar $ Observing False 3 $ Just $ PathFound pathVar
      addAnimatedObject animatedObjectsVar pId $ AnimatedPath pathVar
    ------------------------------


    Observing b n mbRes | n > 0 -> writeTVar currentActivityVar $ Observing b (n - 1) mbRes

    Observing _ n mbRes | n <= 0 -> do
      case mbRes of
        Nothing                     -> writeTVar currentActivityVar $ Idling idlingPeriod
        Just (PathFound actPathVar) -> writeTVar currentActivityVar $ FollowingPath actPathVar

    FollowingPath actPathVar -> do
      ActivePath pId path mbPathDisp <- readTVar actPathVar
      case path of
        [] -> do
          removeAnimatedObject animatedObjectsVar pId
          writeTVar currentActivityVar $ Idling idlingPeriod
        (p:ps)  -> do
          moveActor actorSt p
          writeTVar actPathVar $ ActivePath pId ps mbPathDisp

simpleGameSimulator :: Float -> GameState -> IO GameState
simpleGameSimulator _ st@(GameState {..}) = do
  atomically $ do
    evaluateActorActivity st playerActorState
    animate animatedObjectsVar
  pure st
