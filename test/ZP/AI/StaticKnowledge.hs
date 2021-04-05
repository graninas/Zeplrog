module ZP.AI.StaticKnowledge where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.AI.Types

import qualified Data.Map as Map
import qualified Data.Set as Set

inventoryPropType :: PropertyType
inventoryPropType = PropertyType "inventory"

actionPropType :: PropertyType
actionPropType = PropertyType "action"

abstractGoalPropType :: PropertyType
abstractGoalPropType = PropertyType "abstract goal"

knownActingObjectsPropType :: PropertyType
knownActingObjectsPropType = PropertyType "known acting objects"

abstractGoalEssence = Essence "abstract goal"


observingEssence     = Essence "observing"
discoveringEssence   = Essence "discovering"
settingGoalsEssence  = Essence "setting goals"
planningEssence      = Essence "planning"
followingPlanEssence = Essence "following a plan"

noActionEssence = Essence "no action"

posEssence      = Essence "pos"
hpEssence       = Essence "hp"
