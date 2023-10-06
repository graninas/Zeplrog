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

setCurrentAction :: ActingObject -> ActiveProperty -> STM ()
setCurrentAction self@(ActingObject {currentActionVar}) actProp@(ActiveProperty {staticProperty}) = do
  writeTVar currentActionVar actProp
  report self $ "Action set: " <> show (essence staticProperty)

selectNextAction'' :: ActingObject -> Essence -> STM ()
selectNextAction'' self@(ActingObject {rootProperty, currentActionVar}) ess = do
  -- FIXME: Inefficient active property search. Can be optimized.
  actProps <- getPropertiesOfType rootProperty actionsPropType
  case find (essenceIs ess) actProps of
    Nothing -> report self $ "Action property not found for essence: " <> show ess
    Just actProp -> setCurrentAction self actProp
  where
    essenceIs :: Essence -> ActiveProperty -> Bool
    essenceIs ess ActiveProperty {staticProperty} = essence staticProperty == ess

selectNextAction :: ActingObject -> STM ()
selectNextAction self@(ActingObject {currentActionVar}) = do
  ActiveProperty{propertyValueVar} <- readTVar currentActionVar
  propVal <- readTVar propertyValueVar
  case propVal of
    PairValue (EssenceValue nextActEssence) _ -> selectNextAction'' self nextActEssence
    _ -> report self "selectNextAction': no next action"

-- Property values for actions are treated as input parameters of those actions.
selectNextActionForObjName :: ZPNet -> ActingObjectName -> STM ()
selectNextActionForObjName zpNet objName = do
  mbActObj <- getActingObject zpNet objName
  case mbActObj of
    Nothing  -> reportGlobal zpNet $ "Acting object not found: " <> show objName
    Just obj -> selectNextAction obj


observe :: ZPNet -> ActingObject -> STM [ActingObject]
observe zpNet@(ZPNet {actingObjects, worldVar}) _ = do
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


evaluateObservingAction :: ZPNet -> ActingObject -> STM ()
evaluateObservingAction zpNet actObj = do
  objs <- observe zpNet actObj
  mapM_ (addForDiscover actObj) objs

evaluateDiscoveringAction :: ZPNet -> ActingObject -> ActiveProperty -> STM ()
evaluateDiscoveringAction zpNet self (ActiveProperty {propertyValueVar}) = do
  propVal <- readTVar propertyValueVar
  case propVal of
    PairValue nextEss (ListValue objsForDiscover) -> do
      mapM_ (\(ActingObjectValue o) -> discover zpNet self o) objsForDiscover
      writeTVar propertyValueVar $ PairValue nextEss $ ListValue []
    _ -> pure ()


evaluateGoalSettingAction :: ZPNet -> ActingObject -> STM ()
evaluateGoalSettingAction _ self = report self "Goal setting action"

evaluatePlanningAction :: ZPNet -> ActingObject -> STM ()
evaluatePlanningAction _ self = report self "Planning action"

evaluateCurrentAction :: ZPNet -> ActingObject -> STM ()
evaluateCurrentAction zpNet@(ZPNet {..}) self@(ActingObject {..}) = do
  actProp <- readTVar currentActionVar
  case actProp of
    ActiveProperty {staticProperty} -> case () of
      () | essence staticProperty == observingEssence    -> evaluateObservingAction zpNet self
      () | essence staticProperty == discoveringEssence  -> evaluateDiscoveringAction zpNet self actProp
      () | essence staticProperty == settingGoalsEssence -> evaluateGoalSettingAction zpNet self
      () | essence staticProperty == planningEssence     -> evaluatePlanningAction zpNet self
      _ -> report self $ "Action is not yet supported: " <> show (essence staticProperty)

evaluateCurrentActionForObjName :: ZPNet -> ActingObjectName -> STM ()
evaluateCurrentActionForObjName zpNet objName = do
  mbActObj <- getActingObject zpNet objName
  case mbActObj of
    Nothing  -> reportGlobal zpNet $ "Acting object not found: " <> show objName
    Just obj -> evaluateCurrentAction zpNet obj
