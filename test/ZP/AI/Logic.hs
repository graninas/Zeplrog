{-# LANGUAGE DuplicateRecordFields #-}

module ZP.AI.Logic where

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

setCurrentAction :: ActingObject -> ActiveProperty -> STM String
setCurrentAction actObj@(ActingObject {currentActionVar}) actProp@(ActiveProperty {staticProperty}) = do
  writeTVar currentActionVar actProp
  pure $ "Action set: " <> show (essence staticProperty)

selectAction' :: ZPNet -> ActingObject -> STM String
selectAction'
  aiNet@(ZPNet {rndSource})
  actObj@(ActingObject {rootProperty, currentActionVar}) = do

    actProps <- getPropertiesOfType rootProperty actionPropType

    let propsCnt = length actProps

    decision <- getRandomValue rndSource propsCnt
    -- No check for validity!
    -- (decision < 0) || (decision >= (length actProps))

    case actProps of
      [] -> pure "No action properties found."
      _  -> setCurrentAction actObj $ actProps !! decision

selectAction :: ZPNet -> ActingObjectName -> STM String
selectAction aiNet@(ZPNet {actingObjectsByName}) name = do
  case Map.lookup name actingObjectsByName of
    Nothing  -> pure $ "Acting object not found: " <> show name
    Just obj -> selectAction' aiNet obj


-- Property values for actions are treated as input parameters of those actions.
selectNextAction :: ZPNet -> ActingObjectName -> STM String
selectNextAction aiNet@(ZPNet {actingObjectsByName}) name = do
  case Map.lookup name actingObjectsByName of
    Nothing  -> pure $ "Acting object not found: " <> show name
    Just obj -> selectAction' aiNet obj

--------
traceStep :: String -> ActiveProperty -> STM ()
traceStep stepName ActiveProperty {..} = do
  let ess = essence staticProperty
  let statPropId = staticPropertyId staticProperty
  trace (stepName <> ", essence: " <> show ess
      <> ", actObjId: " <> show activePropertyId
      <> ", statPropId: " <> show statPropId)
    $ pure ()


observe :: ZPNet -> ActingObject -> STM [ActingObject]
observe aiNet@(ZPNet {actingObjects, worldVar}) _ = do
  -- Initial observing logic.
  -- TODO: use more realistic observing algorithm.
  World {worldObjects} <- readTVar worldVar
  let objIds = map worldObjectId worldObjects
  pure $ Map.elems actingObjects

isAlreadyKnownActingObject :: ActingObject -> ActingObject -> STM Bool
isAlreadyKnownActingObject self ActingObject{actingObjectId} = do
  knownObjs <- readTVar $ knownActingObjectsVar self
  pure $ Map.member actingObjectId knownObjs


discoverPropertiesByTypes
  :: (PropertyType, TVar [ActiveProperty])
  -> STM (PropertyType, [KnownActiveProperty])
discoverPropertiesByTypes (propType, activePropsVar) = do
  trace ("discoverPropertiesByTypes" <> show propType) $ pure ()
  activeProps <- readTVar activePropsVar
  mbProps     <- mapM discoverProperty activeProps
  pure (propType, catMaybes mbProps)

discoverChunk :: ActiveProperty -> STM KnownActiveProperty
discoverChunk activeProp@(ActiveProperty {propertiesVar, propertyValue, staticProperty}) = do
  traceStep "discoverChunk" activeProp
  activeProps   <- readTVar propertiesVar
  knownProps    <- mapM discoverPropertiesByTypes $ Map.toList activeProps
  knownPropsVar <- newTVar $ Map.fromList knownProps
  knownVal      <- case (activeValueDiscover staticProperty, propertyValue) of
    (ActiveValueDiscoverable, Just valVar) -> readTVar valVar >>= newTVar >>= pure . Just
    (ActiveValueNonDiscoverable, _) -> pure Nothing
  pure $ KnownActiveProperty activeProp knownPropsVar knownVal


discoverProperty :: ActiveProperty -> STM (Maybe KnownActiveProperty)
discoverProperty activeProp@(ActiveProperty{staticProperty}) =
  case () of
    () | staticPropertyDiscover staticProperty == StaticDiscoverRoot
        -> Just <$> discoverChunk activeProp

    () | staticPropertyDiscover staticProperty == StaticDiscoverLeaf
        -> Just <$> discoverChunk activeProp

    () | staticPropertyDiscover staticProperty == StaticNonDiscoverable
        -> pure Nothing

discover' :: ActingObject -> ActingObject -> STM ()
discover' ActingObject{knownActingObjectsVar} other@(ActingObject{actingObjectId}) = do
  mbKnownProp <- discoverProperty $ rootProperty other
  case mbKnownProp of
    Nothing -> pure ()
    Just knownProp -> do
      knownObjs <- readTVar knownActingObjectsVar
      let knownObj = KnownActingObject actingObjectId knownProp
      writeTVar knownActingObjectsVar $ Map.insert actingObjectId knownObj knownObjs
      trace "discover': discoverPropety returned prop" $ pure ()

discover :: ZPNet -> ActingObject -> ActingObject -> STM ()
discover _ self other | actingObjectId self == actingObjectId other = do
  trace "discover: discovering self not needed." $ pure ()
discover aiNet self other = do
  known <- isAlreadyKnownActingObject self other
  when (not known) $ do
    trace "discover: discovering acting object" $ pure ()
    discover' self other
  when known $ trace "discover: discovering known object not needed." $ pure ()


evaluateObservingAction :: ZPNet -> ActingObject -> STM String
evaluateObservingAction aiNet@(ZPNet {..}) actObj@(ActingObject {..}) = do
  objs <- observe aiNet actObj
  mapM_ (discover aiNet actObj) objs
  pure "Observing action"

evaluateGoalSettingAction :: ZPNet -> ActingObject -> STM String
evaluateGoalSettingAction _ _ = pure "Goal setting action"

evaluatePlanningAction :: ZPNet -> ActingObject -> STM String
evaluatePlanningAction _ _ = pure "Planning action"

evaluateCurrentAction' :: ZPNet -> ActingObject -> STM String
evaluateCurrentAction' aiNet@(ZPNet {..}) actObj@(ActingObject {..}) = do
  actProp <- readTVar currentActionVar
  case actProp of
    ActiveProperty {staticProperty} -> case () of
      () | essence staticProperty == observingEssence    -> evaluateObservingAction aiNet actObj
      () | essence staticProperty == settingGoalsEssence -> evaluateGoalSettingAction aiNet actObj
      () | essence staticProperty == planningEssence     -> evaluatePlanningAction aiNet actObj
      _ -> pure $ "Action is not yet supported: " <> show (essence staticProperty)

evaluateCurrentAction :: ZPNet -> ActingObjectName -> STM String
evaluateCurrentAction aiNet@(ZPNet {actingObjectsByName}) objName = do
  case Map.lookup objName actingObjectsByName of
    Nothing  -> pure $ "Acting object not found: " <> show objName
    Just obj -> evaluateCurrentAction' aiNet obj
