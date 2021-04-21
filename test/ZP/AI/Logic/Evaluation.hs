{-# LANGUAGE DuplicateRecordFields #-}

module ZP.AI.Logic.Evaluation where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic
import ZP.AI.Types
import ZP.AI.Materialization
import ZP.AI.StaticKnowledge
import ZP.AI.Logic.Common
import ZP.AI.Logic.Discovering
import ZP.AI.Logic.Observing
import ZP.AI.Logic.Activation
import ZP.AI.Logic.SettingGoals

import Debug.Trace (trace)

import qualified Data.Map as Map
import qualified Data.Set as Set


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
    PairValue (EssenceValue _ nextActEssence) _ -> selectNextAction'' self nextActEssence
    _ -> report self "selectNextAction': no next action"

-- Property values for actions are treated as input parameters of those actions.
selectNextActionForObjName :: ZPNet -> ActingObjectName -> STM ()
selectNextActionForObjName zpNet objName = do
  mbActObj <- getActingObject zpNet objName
  case mbActObj of
    Nothing  -> reportGlobal zpNet $ "Acting object not found: " <> show objName
    Just obj -> selectNextAction obj


evaluatePlanningAction :: ZPNet -> ActingObject -> STM ()
evaluatePlanningAction _ self = report self "Planning action"


evaluateCurrentAction :: ZPNet -> ActingObject -> STM ()
evaluateCurrentAction zpNet@(ZPNet {..}) self@(ActingObject {..}) = do
  actProp <- readTVar currentActionVar
  case actProp of
    ActiveProperty {staticProperty} -> case () of
      () | essence staticProperty == observingEssence    -> evaluateObservingAction zpNet self
      () | essence staticProperty == discoveringEssence  -> evaluateDiscoveringAction zpNet self actProp
      () | essence staticProperty == settingGoalsEssence -> evaluateGoalsSettingAction zpNet self
      () | essence staticProperty == planningEssence     -> evaluatePlanningAction zpNet self
      _ -> report self $ "Action is not yet supported: " <> show (essence staticProperty)

evaluateCurrentActionForObjName :: ZPNet -> ActingObjectName -> STM ()
evaluateCurrentActionForObjName zpNet objName = do
  mbActObj <- getActingObject zpNet objName
  case mbActObj of
    Nothing  -> reportGlobal zpNet $ "Acting object not found: " <> show objName
    Just obj -> evaluateCurrentAction zpNet obj
