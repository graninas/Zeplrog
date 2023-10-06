{-# LANGUAGE DuplicateRecordFields #-}

module ZP.AI.Logic.Conditions where

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


-- TODO

evaluateConditionProperty :: ZPNet -> ActingObject -> ActiveProperty -> STM Bool
evaluateConditionProperty zpNet actObj condProperty =
  pure True
