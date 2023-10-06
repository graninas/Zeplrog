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


openStateStaticProperty :: CommonStaticProperties -> KBBuilder StaticProperty
openStateStaticProperty CommonStaticProperties{..} = do
  -- TODO: passable property & discoverability
  -- Essence description == action name to activate the target
  let nextStateEssenceVal = EssenceValue "close" closedEssence
  mkStaticProperty openEssence Map.empty nextStateEssenceVal StaticDiscoverRoot ActiveValueNonDiscoverable


closedStateStaticProperty :: CommonStaticProperties -> KBBuilder StaticProperty
closedStateStaticProperty CommonStaticProperties{..} = do
  -- TODO: passable property & discoverability
  -- Essence description == action name to activate the target
  let nextStateEssenceVal = EssenceValue "open" openEssence
  mkStaticProperty closedEssence Map.empty nextStateEssenceVal StaticDiscoverRoot ActiveValueNonDiscoverable


openingConditionStaticProperty :: CommonStaticProperties -> StaticProperty -> KBBuilder StaticProperty
openingConditionStaticProperty csp stateSProp = do
  let props = Map.singleton targetPropType [stateSProp]
  -- TODO: condition value
  -- NoValue == no condition
  mkStaticProperty conditionEssence props NoValue StaticDiscoverRoot ActiveValueNonDiscoverable

closingConditionStaticProperty :: CommonStaticProperties -> StaticProperty -> KBBuilder StaticProperty
closingConditionStaticProperty csp stateSProp = do
  let props = Map.singleton targetPropType [stateSProp]
  -- TODO: condition value
  -- NoValue == no condition
  mkStaticProperty conditionEssence props NoValue StaticDiscoverRoot ActiveValueNonDiscoverable



doorStaticProperty :: CommonStaticProperties -> KBBuilder StaticProperty
doorStaticProperty csp@(CommonStaticProperties{posSProp, hpSProp}) = do
  openStateSProp   <- openStateStaticProperty csp
  closedStateSProp <- closedStateStaticProperty csp

  openingCondSProp <- openingConditionStaticProperty csp openStateSProp
  closingCondSProp <- closingConditionStaticProperty csp closedStateSProp

  -- Initial value (points to the door state itself, not to its condition)
  let currentState = StateValue $ StaticPropertyValue openStateSProp
  -- StateValue => object is interactible
  -- Possible actions via statesPropType

  -- In runtime, propertyValueVar holds the current state

  let props = Map.fromList
        [ (statesPropType,    [ openingCondSProp, closingCondSProp ])
        , (inventoryPropType, [ posSProp, hpSProp ])
        ]

  mkStaticProperty doorEssence props currentState StaticDiscoverRoot ActiveValueDiscoverable
