module ZP.AI.StaticKnowledge where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.AI.Types

import qualified Data.Map as Map
import qualified Data.Set as Set

inventoryPropType :: PropertyType
inventoryPropType = PropertyType "inventory"

actionsPropType :: PropertyType
actionsPropType = PropertyType "actions"

goalsPropType :: PropertyType
goalsPropType = PropertyType "goals"

-- targetPropType :: PropertyType
-- targetPropType = PropertyType "target"

activationsPropType :: PropertyType
activationsPropType = PropertyType "activations"

statesPropType :: PropertyType
statesPropType = PropertyType "states"

knownActingObjectsPropType :: PropertyType
knownActingObjectsPropType = PropertyType "known acting objects"

goalEssence = Essence "goal"

conditionEssence = Essence "condition"


noActionEssence      = Essence "no action"
observingEssence     = Essence "observing"
discoveringEssence   = Essence "discovering"
settingGoalsEssence  = Essence "setting goals"
planningEssence      = Essence "planning"
followingPlanEssence = Essence "following a plan"


posEssence      = Essence "pos"
hpEssence       = Essence "hp"


doorEssence = Essence "door"
openEssence = Essence "open"
closedEssence = Essence "closed"
