{-# LANGUAGE DuplicateRecordFields #-}

module ZP.AI.Logic.Observing where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic
import ZP.AI.Types
import ZP.AI.Materialization
import ZP.AI.StaticKnowledge
import ZP.AI.Logic.Common

import Debug.Trace (trace)

import qualified Data.Map as Map
import qualified Data.Set as Set


addForDiscover :: ActingObject -> ActingObject -> STM ()
addForDiscover self@(ActingObject {actionsByEssenceVar}) other = do
  acts <- readTVar actionsByEssenceVar
  case Map.lookup discoveringEssence acts of
    Nothing -> pure ()                              -- may happen (self is incapable of discovering)
    Just (ActiveProperty {propertyValueVar}) -> do
      propVal <- readTVar propertyValueVar
      case propVal of
        PairValue nextEss (ListValue objsForDiscover) -> do
          let objs = ActingObjectValue other : objsForDiscover
          writeTVar propertyValueVar $ PairValue nextEss $ ListValue objs
        PairValue nextEss NoValue ->
          writeTVar propertyValueVar $ PairValue nextEss $ ListValue [ActingObjectValue other]
        _ -> pure ()      -- should not happen (a bug in data or logic)


observe :: ZPNet -> ActingObject -> STM [ActingObject]
observe zpNet@(ZPNet {actingObjects, worldVar}) _ = do
  -- Initial observing logic.
  -- TODO: use more realistic observing algorithm.
  World {worldObjects} <- readTVar worldVar
  let objIds = map worldObjectId worldObjects
  pure $ Map.elems actingObjects



evaluateObservingAction :: ZPNet -> ActingObject -> STM ()
evaluateObservingAction zpNet actObj = do
  objs <- observe zpNet actObj
  mapM_ (addForDiscover actObj) objs
