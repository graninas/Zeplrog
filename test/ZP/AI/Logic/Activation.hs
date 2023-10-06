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
import ZP.AI.Logic.Conditions
import ZP.AI.Debug

import qualified Data.Map as Map
import qualified Data.Set as Set

type ActivatonName = String
type Activation = Map.Map ActivatonName ActiveProperty


getActivations :: ActiveProperty -> STM Activation
getActivations prop = do
  props <- getPropertiesOfType prop activationsPropType
  pure $ Map.fromList $ map (\prop -> (activePropertyDescription prop, prop)) props


getAllActivations :: ActingObject -> STM Activation
getAllActivations actObj = do
  props <- getPropertiesOfType (rootProperty actObj) statesPropType
  activations' <- mapM getActivations props
  pure $ Map.unions activations'

getCurrentActivations :: ActingObject -> STM Activation
getCurrentActivations actObj = do
  curStatePropVal <- readTVar $ propertyValueVar $ rootProperty actObj
  case curStatePropVal of
    ActivePropertyValue _ curStateProp -> getActivations curStateProp
    _ -> do
      report actObj "Malformed state object: unknown property value."
      pure Map.empty


-- Active Property should be a condition property
setNewState :: Padding -> ActingObject -> ActiveProperty -> STM (Maybe ActiveProperty)
setNewState pad actObj condProp = do
  outputDbg $ pad <> "setNewState"
  case conditionEssence == (essence $ staticProperty condProp) of
    False -> do
      outputDbg $ pad <> "-- invalid proprty to activate: not a condition property."
      report actObj "Invalid proprty to activate: not a condition property."
      pure Nothing
    True -> do
      conditionPropVal <- readTVar $ propertyValueVar condProp
      case conditionPropVal of
        ActivePropertyValue _ targetStateProp -> do
          outputDbg $ pad <> "target state prop found, setting..."
          writeTVar (propertyValueVar $ rootProperty actObj)
            $ ActivePropertyValue "cur state" targetStateProp
          pure $ Just targetStateProp
        _ -> do
          outputDbg $ pad <> "Malformed condition proprty: unknown property value."
          report actObj "Malformed condition proprty: unknown property value."
          pure Nothing



activate :: ZPNet -> ActingObject -> ActivatonName -> STM (Maybe ActiveProperty)
activate zpNet actObj activationName = do
  curActivations <- getCurrentActivations actObj
  outputDbg "activate"
  outputDbg $ "-- number of activations: " <> (show $ Map.size curActivations)
  case Map.lookup activationName curActivations of
    Nothing -> do
      outputDbg $ "-- activation not found: " <> show activationName
      pure Nothing
    Just condProp -> do
      outputDbg $ "-- activation found, evaluating condition..."
      condHolds <- evaluateConditionProperty zpNet actObj condProp
      if condHolds
        then do
          outputDbg $ "-- condition holds, setting the new state..."
          setNewState "  " actObj condProp
        else do
          outputDbg $ "-- condition does not hold, finishing."
          pure Nothing


getCurrentStateProperty :: ActingObject -> STM (Maybe ActiveProperty)
getCurrentStateProperty actObj = do
  val <- readTVar $ propertyValueVar $ rootProperty actObj
  case val of
    ActivePropertyValue _ curStateProp -> pure $ Just curStateProp
    _ -> do
      report actObj "Malformed state object: unknown state value."
      pure Nothing
