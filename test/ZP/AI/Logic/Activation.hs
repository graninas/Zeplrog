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


getAllActivations :: ActingObject -> STM [(Description, Essence)]
getAllActivations actObj = do
  props <- getPropertiesOfType (rootProperty actObj) statesPropType
  traceM $ "states count:" <> (show $ length props)
  activations' <- mapM getActivation' props
  pure $ join activations'
  where
    -- Receives a condition property which points to its targets.
    -- (There can be many targets of activation)
    -- The target property has a value with activation
    getActivation' :: ActiveProperty -> STM [(Description, Essence)]
    getActivation' prop = do
      targets <- getPropertiesOfType (rootProperty actObj) targetPropType
      xs <- mapM getActivationName targets
      traceM $ "targets: " <> show xs
      pure $ catMaybes xs
    getActivationName :: ActiveProperty -> STM (Maybe (Description, Essence))
    getActivationName (ActiveProperty {propertyValueVar}) = do
      val <- readTVar propertyValueVar
      case val of
        EssenceValue aN aTN -> pure $ Just (aN, aTN)
        _ -> do
          report actObj $ "Actinvation object is malformed: unknown property value"
          pure $ Nothing
