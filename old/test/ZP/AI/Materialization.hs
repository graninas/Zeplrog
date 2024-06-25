{-# LANGUAGE DuplicateRecordFields #-}

module ZP.AI.Materialization where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic
import ZP.AI.Types
import ZP.AI.Debug

import qualified Data.Map as Map
import qualified Data.Set as Set

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


updateDelayedVars
  :: Padding
  -> DelayedVariables
  -> MaterializedProperties
  -> STM ()
updateDelayedVars pad delayedVarsVar matPropsVar = do
  outputDbg $ pad <> "updateDelayedVars"
  delayedVars <- readTVar delayedVarsVar
  matProps' <- readTVar matPropsVar
  let matProps = Map.toList matProps'

  -- TODO: it's possible that prop is not the needed prop
  -- if several props on this static id were created
  let f (sPropId, prop) = do
          case Map.lookup sPropId delayedVars of
            Nothing -> do
              outputDbg $ pad <> "-- prop is not found for delayed var for sPropId: " <> show sPropId
              pure ()
            Just vars -> do
              outputDbg $ pad <> "-- prop is found for delayed var for sPropId: " <> show sPropId
              mapM_ (\(descr, var) -> writeTVar var $ ActivePropertyValue descr prop) vars

  mapM_ f matProps

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
  actProps <- mapM (materializeLink' pad idCounterVar kb matPropsVar delayedVarsVar propsSetter) matLinks
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
  sProp@(StaticProperty {staticProperties, staticPropertyDescription, staticPropertyValueVar}) = do
    let ess = essence sProp
    outputDbg $ pad <> "materializeStaticProperty"
    outputDbg $ pad <> "-- sProp: " <> show (staticPropertyId sProp) <> " essence: " <> show ess

    props <- mapM (materializeLinksByType (pad <> "  ") idCounterVar kb matPropsVar delayedVarsVar propsSetter)
                $ Map.toList staticProperties

    propsVar <- newTVar $ Map.fromList props
    propId <- getActivePropertyId idCounterVar

    sPropVal <- readTVar staticPropertyValueVar
    propValVar <- case sPropVal of

      -- Checking for externally defined values
      NoStaticValue -> case Map.lookup ess propsSetter of
        Nothing  -> newTVar NoValue
        Just val -> newTVar val

      -- Materializing the static property into the variable
      -- (ignoring externally defined values).
      -- May cause infinite materialization loops if the source data is done wrongly.
      MaterializableStateValue descr (DirectMaterialization linkedSProp) -> do
        outputDbg $ pad <> "-- state val, direct materialization. Linked sProp: "
            <> show (staticPropertyId linkedSProp)
            <> " essence: " <> show (essence linkedSProp)
        outputDbg $ pad <> "-- materializing"
        linkedProp <- materializeStaticProperty (pad <> "  ") idCounterVar kb matPropsVar delayedVarsVar propsSetter linkedSProp
        newTVar $ ActivePropertyValue descr linkedProp

      -- The same, just delaying the materialization.
      -- This should prevent for loops.
      MaterializableStateValue descr (SharedInsterialization linkedSProp) -> do
        outputDbg $ pad <> "-- state val, shared materialization. Linked sProp: "
            <> show (staticPropertyId linkedSProp)
            <> " essence: " <> show (essence linkedSProp)
        outputDbg $ pad <> "-- storing delayed var"
        let linkedSPropId = staticPropertyId linkedSProp
        delayedVar <- newTVar NoValue
        delayedVars <- readTVar delayedVarsVar
        writeTVar delayedVarsVar $ Map.insertWith (++) linkedSPropId [(descr, delayedVar)] delayedVars
        pure delayedVar

    outputDbg $ pad <> "-- active property created. propId: " <> show propId
    let prop = ActiveProperty propId staticPropertyDescription sProp propValVar propsVar

    -- Store brand new active property for the future
    outputDbg $ pad <> "-- storing active property into materialized props"
    modifyTVar' matPropsVar $ Map.insert (staticPropertyId sProp) prop

    pure prop

materializeLink'
  :: Padding
  -> IdCounter
  -> KnowledgeBase
  -> MaterializedProperties
  -> DelayedVariables
  -> PropertiesSetter
  -> MaterializationLink
  -> STM ActiveProperty
materializeLink' pad idCounterVar kb matPropsVar delayedVarsVar propsSetter matLink = do
  outputDbg $ pad <> "materializeLink'"

  case matLink of

    -- Materialize the static property into the new active property.
    DirectMaterialization sProp -> do
      outputDbg $ pad <> "-- direct mat, sProp: "
        <> show (staticPropertyId sProp)
        <> " essence: " <> show (essence sProp)
      matProp <- materializeStaticProperty (pad <> "  ") idCounterVar kb matPropsVar delayedVarsVar propsSetter sProp
      modifyTVar' matPropsVar $ Map.insert (staticPropertyId sProp) matProp
      pure matProp

    -- If static property is already materialized, search for it.
    -- Otherwise, materialize into a new active property and remember it for the future.
    SharedInsterialization sProp -> do
      outputDbg $ pad <> "-- shared mat, sProp: "
        <> show (staticPropertyId sProp)
        <> " essence: " <> show (essence sProp)
      mProps <- readTVar matPropsVar      -- already materialized props
      case Map.lookup (staticPropertyId sProp) mProps of
        Just matProp -> do
          outputDbg $ pad <> "-- -- found mat prop: " <> show (staticPropertyId sProp)
          pure matProp
        Nothing -> do
          outputDbg $ pad <> "-- -- mat prop not found. Materializing"
          materializeStaticProperty (pad <> "  ") idCounterVar kb matPropsVar delayedVarsVar propsSetter sProp


materializeLink
  :: Padding
  -> IdCounter
  -> KnowledgeBase
  -> MaterializedProperties
  -> DelayedVariables
  -> PropertiesSetter
  -> MaterializationLink
  -> STM ActiveProperty
materializeLink pad idCounterVar kb matPropsVar delayedVarsVar propsSetter matLink = do
  outputDbg $ pad <> "materializeLink"

  prop <- materializeLink' (pad <> "  ") idCounterVar kb matPropsVar delayedVarsVar propsSetter matLink

  -- Search for delayed vars and update
  outputDbg $ pad <> "-- checking delayed vars"
  updateDelayedVars (pad <> "  ") delayedVarsVar matPropsVar

  pure prop
