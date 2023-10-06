{-# LANGUAGE DuplicateRecordFields #-}

module ZP.AI.Materialization where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic
import ZP.AI.Types

import qualified Data.Map as Map
import qualified Data.Set as Set


type MaterializedProperties = TVar (Map.Map StaticPropertyId ActiveProperty)

getActingObjectId :: IdCounter -> STM ActingObjectId
getActingObjectId idCounterVar = do
  propId <- getId idCounterVar
  pure $ ActingObjectId propId

getActivePropertyId :: IdCounter -> STM ActivePropertyId
getActivePropertyId idCounterVar = do
  propId <- getId idCounterVar
  pure $ ActivePropertyId propId


materializeLinksByType
  :: IdCounter
  -> KnowledgeBase
  -> MaterializedProperties
  -> PropertiesSetter
  -> (PropertyType, [MaterializationLink])
  -> STM (PropertyType, TVar [ActiveProperty])
materializeLinksByType idCounterVar kb matPropsVar propsSetter (pType, matLinks)  = do
  actProps <- mapM (materializeLink idCounterVar kb matPropsVar propsSetter) matLinks
  actPropsVar <- newTVar actProps
  pure (pType, actPropsVar)

materializeLink
  :: IdCounter
  -> KnowledgeBase
  -> MaterializedProperties
  -> PropertiesSetter
  -> MaterializationLink
  -> STM ActiveProperty
materializeLink idCounterVar kb matPropsVar propsSetter matLink =
  case matLink of
    DirectMaterialization sProp -> do
      matProp <- materializeStaticProperty idCounterVar kb matPropsVar propsSetter sProp
      modifyTVar' matPropsVar $ Map.insert (staticPropertyId sProp) matProp
      pure matProp
    SharedMaterialization sProp -> do
      mProps <- readTVar matPropsVar
      case Map.lookup (staticPropertyId sProp) mProps of
        Just matProp -> pure matProp
        Nothing -> do
          matProp <- materializeStaticProperty idCounterVar kb matPropsVar propsSetter sProp
          modifyTVar' matPropsVar $ Map.insert (staticPropertyId sProp) matProp
          pure matProp

materializeStaticProperty
  :: IdCounter
  -> KnowledgeBase
  -> MaterializedProperties
  -> PropertiesSetter
  -> StaticProperty
  -> STM ActiveProperty
materializeStaticProperty
  idCounterVar kb matPropsVar
  propsSetter
  sProp@(StaticProperty {essence, staticProperties}) = do
    propValVar <- newTVar $ case Map.lookup essence propsSetter of
      Nothing  -> NoValue
      Just val -> val
    props <- mapM (materializeLinksByType idCounterVar kb matPropsVar propsSetter)
                $ Map.toList staticProperties
    propsVar <- newTVar $ Map.fromList props
    propId <- getActivePropertyId idCounterVar
    pure $ ActiveProperty propId sProp propValVar propsVar
