{-# LANGUAGE DuplicateRecordFields #-}

module ZP.AI.Materialization where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic
import ZP.AI.Types

import qualified Data.Map as Map
import qualified Data.Set as Set

-- -----------------------------------------------------------------------------

getActingObjectId :: IdCounter -> STM ActingObjectId
getActingObjectId idCounterVar = do
  propId <- getId idCounterVar
  pure $ ActingObjectId propId

getActivePropertyId :: IdCounter -> STM ActivePropertyId
getActivePropertyId idCounterVar = do
  propId <- getId idCounterVar
  pure $ ActivePropertyId propId


materializeStaticPropertyByType
  :: IdCounter
  -> KnowledgeBase
  -> MaterializedProperties
  -> PropertiesSetter
  -> (PropertyType, [MaterializationLink])
  -> STM (PropertyType, TVar [ActiveProperty])
materializeStaticPropertyByType idCounterVar kb matPropsVar
  propsSetter
  (pType, matLinks)  = do
    undefined
    -- actProps <- mapM (materializeStaticProperty idCounterVar kb matPropsVar propsSetter) matLinks
    -- actPropsVar <- newTVar actProps
    -- pure (pType, actPropsVar)

type MaterializedProperties = TVar (Map.Map StaticPropertyId ActiveProperty)

materializeStaticProperty
  :: IdCounter
  -> KnowledgeBase
  -> MaterializedProperties
  -> PropertiesSetter
  -> MaterializationLink
  -> STM ActiveProperty
materializeStaticProperty idCounterVar kb matPropsVar propsSetter matLink = do
  undefined
  -- matProp <- case matLink of
  --   DirectMaterialization sProp -> materializeStaticProperty' idCounterVar kb matPropsVar propsSetter sProp
  --   SharedMaterialization sProp -> do
  --     mProps <- readTVar matPropsVar
  --     case Map.lookup staticPropertyId mProps of
  --       Just matProp -> pure matProp
  --       Nothing -> materializeStaticProperty' idCounterVar kb matPropsVar propsSetter sProp
  -- modifyTVar' matPropsVar $ Map.insert staticPropertyId matProp
  -- pure matProp

materializeStaticProperty'
  :: IdCounter
  -> KnowledgeBase
  -> MaterializedProperties
  -> PropertiesSetter
  -> StaticProperty
  -> STM ActiveProperty
materializeStaticProperty'
  idCounterVar kb matPropsVar
  propsSetter
  sProp = do
    undefined
    -- propValVar <- newTVar $ case Map.lookup essence propsSetter of
    --   Nothing  -> NoValue
    --   Just val -> val
    -- props <- mapM (materializeStaticPropertyByType idCounterVar kb matPropsVar propsSetter)
    --             $ Map.toList staticProperties
    -- propsVar <- newTVar $ Map.fromList props
    -- propId <- getActivePropertyId idCounterVar
    -- pure $ ActiveProperty propId sProp propValVar propsVar
