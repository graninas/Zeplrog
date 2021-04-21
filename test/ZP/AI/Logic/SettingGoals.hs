{-# LANGUAGE DuplicateRecordFields #-}

module ZP.AI.Logic.SettingGoals where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic
import ZP.AI.Types
import ZP.AI.Materialization
import ZP.AI.StaticKnowledge
import ZP.AI.Logic.Common

import Debug.Trace (trace)

import qualified Data.Map as Map
import qualified Data.Set as Set




evaluateGoalsSettingAction :: ZPNet -> ActingObject -> STM ()
evaluateGoalsSettingAction _ self = do


  report self "Goals setting action"
