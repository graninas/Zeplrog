{-# LANGUAGE DuplicateRecordFields #-}

module ZP.AI.Logic.Discovering where

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


discoverPropertiesByTypes
  :: ActingObject
  -> (PropertyType, TVar [ActiveProperty])
  -> STM (PropertyType, [KnownActiveProperty])
discoverPropertiesByTypes self (propType, activePropsVar) = do
  report self $ "discoverPropertiesByTypes" <> show propType
  activeProps <- readTVar activePropsVar
  mbProps     <- mapM (discoverProperty self) activeProps
  pure (propType, catMaybes mbProps)

discoverChunk :: ActingObject -> ActiveProperty -> STM KnownActiveProperty
discoverChunk self activeProp@(ActiveProperty {propertiesVar, propertyValueVar, staticProperty}) = do
  reportStep self "discoverChunk" activeProp
  activeProps   <- readTVar propertiesVar
  knownProps    <- mapM (discoverPropertiesByTypes self) $ Map.toList activeProps
  knownPropsVar <- newTVar $ Map.fromList knownProps
  mbKnownValVar <- case activeValueDiscover staticProperty of
    ActiveValueNonDiscoverable -> newTVar Nothing
    ActiveValueDiscoverable    -> do
      propVal <- readTVar propertyValueVar
      newTVar $ Just propVal
  pure $ KnownActiveProperty activeProp knownPropsVar mbKnownValVar


discoverProperty :: ActingObject -> ActiveProperty -> STM (Maybe KnownActiveProperty)
discoverProperty self activeProp@(ActiveProperty{staticProperty}) =
  case () of
    () | staticPropertyDiscover staticProperty == StaticDiscoverRoot
        -> Just <$> discoverChunk self activeProp

    () | staticPropertyDiscover staticProperty == StaticDiscoverLeaf
        -> Just <$> discoverChunk self activeProp

    () | staticPropertyDiscover staticProperty == StaticNonDiscoverable
        -> pure Nothing

discover' :: ActingObject -> ActingObject -> STM ()
discover' self@ActingObject{knownActingObjectsVar} other@(ActingObject{actingObjectId}) = do
  mbKnownProp <- discoverProperty self $ rootProperty other
  case mbKnownProp of
    Nothing -> pure ()
    Just knownProp -> do
      knownObjs <- readTVar knownActingObjectsVar
      let knownObj = KnownActingObject actingObjectId knownProp
      writeTVar knownActingObjectsVar $ Map.insert actingObjectId knownObj knownObjs
      report self "discover': discoverPropety returned prop"

discover :: ZPNet -> ActingObject -> ActingObject -> STM ()
discover _ self other | actingObjectId self == actingObjectId other = do
  report self "discover: discovering self not needed."
discover zpNet self other = do
  known <- isAlreadyKnownActingObject self other
  when (not known) $ do
    report self "discover: discovering acting object"
    discover' self other
  when known
    $ report self "discover: discovering known object not needed."

evaluateDiscoveringAction :: ZPNet -> ActingObject -> ActiveProperty -> STM ()
evaluateDiscoveringAction zpNet self (ActiveProperty {propertyValueVar}) = do
  propVal <- readTVar propertyValueVar
  case propVal of
    PairValue nextEss (ListValue objsForDiscover) -> do
      mapM_ (\(ActingObjectValue o) -> discover zpNet self o) objsForDiscover
      writeTVar propertyValueVar $ PairValue nextEss $ ListValue []
    _ -> pure ()
