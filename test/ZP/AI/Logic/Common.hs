{-# LANGUAGE DuplicateRecordFields #-}

module ZP.AI.Logic.Common where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic
import ZP.AI.Types
import ZP.AI.Materialization
import ZP.AI.StaticKnowledge

import Debug.Trace (trace)

import qualified Data.Map as Map
import qualified Data.Set as Set


report :: ActingObject -> String -> STM ()
report ActingObject{actingObjectReporter} msg = case actingObjectReporter of
  Nothing -> pure ()
  Just reporterVar -> modifyTVar' reporterVar (msg:)

reportStep :: ActingObject -> String -> ActiveProperty -> STM ()
reportStep self stepName ActiveProperty {..} = do
  let ess = essence staticProperty
  let statPropId = staticPropertyId staticProperty
  report self (stepName <> ", essence: " <> show ess
      <> ", actObjId: " <> show activePropertyId
      <> ", statPropId: " <> show statPropId)

reportGlobal :: ZPNet -> String -> STM ()
reportGlobal ZPNet{zpNetReporter} msg = case zpNetReporter of
  Nothing -> pure ()
  Just reporterVar -> modifyTVar' reporterVar (msg:)

getRandomValue :: RndSource -> Int -> STM Int
getRandomValue rndSource input = rndSource input

getPropertiesOfTypeVar :: ActiveProperty -> PropertyType -> STM (Maybe (TVar [ActiveProperty]))
getPropertiesOfTypeVar (ActiveProperty {..}) propType = do
  props <- readTVar propertiesVar
  pure $ Map.lookup propType props

getPropertiesOfType :: ActiveProperty -> PropertyType -> STM [ActiveProperty]
getPropertiesOfType prop propType = do
  mbVar <- getPropertiesOfTypeVar prop propType
  case mbVar of
    Nothing  -> pure []
    Just var -> readTVar var

getActingObject :: ZPNet -> ActingObjectName -> STM (Maybe ActingObject)
getActingObject zpNet@(ZPNet {actingObjectsByName}) objName =
  pure $ Map.lookup objName actingObjectsByName


isAlreadyKnownActingObject :: ActingObject -> ActingObject -> STM Bool
isAlreadyKnownActingObject self ActingObject{actingObjectId} = do
  knownObjs <- readTVar $ knownActingObjectsVar self
  pure $ Map.member actingObjectId knownObjs
