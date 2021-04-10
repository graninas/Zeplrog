{-# LANGUAGE DuplicateRecordFields #-}

module ZP.AI.Logic.Activation where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic
import ZP.AI.Types
import ZP.AI.Materialization
import ZP.AI.StaticKnowledge
import ZP.AI.Logic.Common

import qualified Data.Map as Map
import qualified Data.Set as Set


getAllActivations :: ActingObject -> STM [(Description, ActiveProperty)]
getAllActivations actObj = do
  props <- getPropertiesOfType (rootProperty actObj) statesPropType
  pure $ map (\prop -> (activePropertyDescription prop, prop)) props

getCurrentActivations :: ActingObject -> STM [(Description, ActiveProperty)]
getCurrentActivations actObj = do
  val <- readTVar $ propertyValueVar $ rootProperty actObj
  case val of
    ActivePropertyValue _ curStateProp -> do
      props <- getPropertiesOfType curStateProp activationsPropType
      pure $ map (\prop -> (activePropertyDescription prop, prop)) props
    _ -> do
      report actObj "Malformed state object: unknown property value."
      pure []
