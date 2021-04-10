{-# LANGUAGE DuplicateRecordFields #-}

module ZP.AI.Materialization where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic
import ZP.AI.Types

import qualified Data.Map as Map
import qualified Data.Set as Set

type Padding = Text

type MaterializedProperties = TVar (Map.Map StaticPropertyId ActiveProperty)

type DelayedVariables = TVar (Map.Map StaticPropertyId [(Description, TVar PropertyValue)])

getActingObjectId :: IdCounter -> STM ActingObjectId
getActingObjectId idCounterVar = do
  propId <- getId idCounterVar
  pure $ ActingObjectId propId

getActivePropertyId :: IdCounter -> STM ActivePropertyId
getActivePropertyId idCounterVar = do
  propId <- getId idCounterVar
  pure $ ActivePropertyId propId


updateDelayedVars :: DelayedVariables -> ActiveProperty -> STM ()
updateDelayedVars delayedVarsVar prop@(ActiveProperty {staticProperty}) = do
  let sPropId = staticPropertyId staticProperty
  delayedVars <- readTVar delayedVarsVar
  case Map.lookup sPropId delayedVars of
    Nothing -> pure ()
    Just vars -> do
      mapM_ (\(descr, var) -> writeTVar var $ ActivePropertyValue descr prop) vars
      writeTVar delayedVarsVar $ Map.delete sPropId delayedVars


materializeLinksByType
  :: Padding
  -> IdCounter
  -> KnowledgeBase
  -> MaterializedProperties
  -> DelayedVariables
  -> PropertiesSetter
  -> (PropertyType, [MaterializationLink])
  -> STM (PropertyType, TVar [ActiveProperty])
materializeLinksByType pad idCounterVar kb matPropsVar delayedVarsVar propsSetter (pType, matLinks)  = do
  actProps <- mapM (materializeLink pad idCounterVar kb matPropsVar delayedVarsVar propsSetter) matLinks
  actPropsVar <- newTVar actProps
  pure (pType, actPropsVar)


materializeStaticProperty
  :: Padding
  -> IdCounter
  -> KnowledgeBase
  -> MaterializedProperties
  -> DelayedVariables
  -> PropertiesSetter
  -> StaticProperty
  -> STM ActiveProperty
materializeStaticProperty
  pad idCounterVar kb matPropsVar delayedVarsVar propsSetter
  sProp@(StaticProperty {essence, staticProperties, staticPropertyValueVar}) = do
    traceM $ pad <> "materializeStaticProperty: sProp: " <> show (staticPropertyId sProp)

    props <- mapM (materializeLinksByType (pad <> "  ") idCounterVar kb matPropsVar delayedVarsVar propsSetter)
                $ Map.toList staticProperties

    propsVar <- newTVar $ Map.fromList props
    propId <- getActivePropertyId idCounterVar

    sPropVal <- readTVar staticPropertyValueVar
    propValVar <- case sPropVal of

      -- Checking for externally defined values
      NoStaticValue -> case Map.lookup essence propsSetter of
        Nothing  -> newTVar NoValue
        Just val -> newTVar val

      -- Materializing the static property into the variable
      -- (ignoring externally defined values).
      -- May cause infinite materialization loops if the source data is done wrongly.
      MaterializableStateValue descr (DirectMaterialization linkedSProp) -> do
        traceM $ pad <> "--state val, direct materialization. Linked sProp: "
            <> show (staticPropertyId linkedSProp)
        traceM $ pad <> "--materializing"
        linkedProp <- materializeStaticProperty (pad <> "  ") idCounterVar kb matPropsVar delayedVarsVar propsSetter linkedSProp
        newTVar $ ActivePropertyValue descr linkedProp

      -- The same, just delaying the materialization.
      -- This should prevent for loops.
      MaterializableStateValue descr (SharedMaterialization linkedSProp) -> do
        traceM $ pad <> "--state val, shared materialization. Linked sProp: "
            <> show (staticPropertyId linkedSProp)
        traceM $ pad <> "--storing delayed var"
        let linkedSPropId = staticPropertyId linkedSProp
        delayedVar <- newTVar NoValue
        delayedVars <- readTVar delayedVarsVar
        writeTVar delayedVarsVar $ Map.insertWith (++) linkedSPropId [(descr, delayedVar)] delayedVars
        pure delayedVar

    let prop = ActiveProperty propId sProp propValVar propsVar

    -- Store brand new active property for the future
    modifyTVar' matPropsVar $ Map.insert (staticPropertyId sProp) prop

    -- Search for delayed vars and update
    traceM $ pad <> "materializeStaticProperty: checking delayed vars"
    updateDelayedVars delayedVarsVar prop
    pure prop

materializeLink
  :: Padding
  -> IdCounter
  -> KnowledgeBase
  -> MaterializedProperties
  -> DelayedVariables
  -> PropertiesSetter
  -> MaterializationLink
  -> STM ActiveProperty
materializeLink pad idCounterVar kb matPropsVar delayedVarsVar propsSetter matLink =
  case matLink of

    -- Materialize the static property into the new active property.
    DirectMaterialization sProp -> do
      traceM $ pad <> "materializeLink: direct mat, sProp: " <> show (staticPropertyId sProp)
      matProp <- materializeStaticProperty (pad <> "  ") idCounterVar kb matPropsVar delayedVarsVar propsSetter sProp
      modifyTVar' matPropsVar $ Map.insert (staticPropertyId sProp) matProp
      pure matProp

    -- If static property is already materialized, search for it.
    -- Otherwise, materialize into a new active property and remember it for the future.
    SharedMaterialization sProp -> do
      traceM $ pad <> "materializeLink: shared mat, sProp: " <> show (staticPropertyId sProp)
      mProps <- readTVar matPropsVar      -- already materialized props
      case Map.lookup (staticPropertyId sProp) mProps of
        Just matProp -> do
          traceM $ pad <> "--found mat prop: " <> show (staticPropertyId sProp)
          pure matProp
        Nothing -> do
          traceM $ pad <> "--materializing"
          materializeStaticProperty (pad <> "  ") idCounterVar kb matPropsVar delayedVarsVar propsSetter sProp
