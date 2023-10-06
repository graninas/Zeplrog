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
  -> PropertiesSetter
  -> (PropertyType, [StaticProperty])
  -> STM (PropertyType, TVar [ActiveProperty])
materializeStaticPropertyByType idCounterVar kb propsSetter (pType, sProps)  = do
  actProps <- mapM (materializeStaticProperty idCounterVar kb propsSetter) sProps
  actPropsVar <- newTVar actProps
  pure (pType, actPropsVar)

materializeStaticProperty
  :: IdCounter
  -> KnowledgeBase
  -> PropertiesSetter
  -> StaticProperty
  -> STM ActiveProperty
materializeStaticProperty idCounterVar kb propsSetter sProp@(StaticProperty {..})  = do
  propId <- getActivePropertyId idCounterVar
  mbPropValVar <- case Map.lookup essence propsSetter of
    Nothing -> pure Nothing
    Just val -> newTVar val >>= pure . Just
  props <- mapM (materializeStaticPropertyByType idCounterVar kb propsSetter)
              $ Map.toList staticProperties
  propsVar <- newTVar $ Map.fromList props
  pure $ ActiveProperty propId sProp mbPropValVar propsVar
