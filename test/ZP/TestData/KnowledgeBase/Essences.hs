{-# LANGUAGE DuplicateRecordFields #-}

module ZP.TestData.KnowledgeBase.Essences where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic
import ZP.AI.Types
import ZP.AI.StaticKnowledge

import qualified Data.Map as Map
import qualified Data.Set as Set

fireWandEssence = Essence "fire wand"
iceWandEssence  = Essence "ice wand"
ratEssence      = Essence "rat"
guardEssence    = Essence "guard"
