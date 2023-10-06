{-# LANGUAGE DuplicateRecordFields #-}

module ZP.TestData.KnowledgeBase.Doors where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic
import ZP.AI.Types
import ZP.AI.StaticKnowledge
import ZP.TestData.KnowledgeBase.Essences
import ZP.TestData.KnowledgeBase.Common

import qualified Data.Map as Map
import qualified Data.Set as Set

--
-- openStaticProperty
--
--
-- doorStaticProperty
--   :: IdCounter
--   -> TVar Essences
--   -> CommonStaticProperties
--   -> STM StaticProperty
-- doorStaticProperty idCounterVar essencesVar CommonStaticProperties{..} = do
--   let props = Map.fromList
--         [ (inventoryPropType, [ posSProp, hpSProp ])
--         , (actionsPropType,   [ noActionSProp ])              -- this rat doesn't do anything...
--         ]
--   mkStaticProperty idCounterVar essencesVar ratEssence props StaticDiscoverRoot ActiveValueNonDiscoverable
