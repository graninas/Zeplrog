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


openStateStaticProperty :: CommonStaticProperties -> StaticProperty -> KBBuilder StaticProperty
openStateStaticProperty CommonStaticProperties{..} closingCondSProp = do
  valVar <- lift $ newTVar NoStaticValue
  let props = Map.singleton activationsPropType [(SharedInsterialization closingCondSProp)]
  -- TODO: passable property & discoverability
  mkStaticProperty openEssence "" props valVar StaticDiscoverRoot ActiveValueNonDiscoverable


closedStateStaticProperty :: CommonStaticProperties -> StaticProperty -> KBBuilder StaticProperty
closedStateStaticProperty CommonStaticProperties{..} openingCondSProp = do
  valVar <- lift $ newTVar NoStaticValue
  let props = Map.singleton activationsPropType [(SharedInsterialization openingCondSProp)]
  -- TODO: passable property & discoverability
  mkStaticProperty closedEssence "" props valVar StaticDiscoverRoot ActiveValueNonDiscoverable


openingConditionStaticProperty :: CommonStaticProperties -> KBBuilder StaticProperty
openingConditionStaticProperty csp = do
  valVar <- lift $ newTVar NoStaticValue
  -- TODO: condition value
  -- NoValue == no condition
  mkStaticProperty conditionEssence "open" Map.empty valVar StaticDiscoverRoot ActiveValueNonDiscoverable

closingConditionStaticProperty :: CommonStaticProperties -> KBBuilder StaticProperty
closingConditionStaticProperty csp = do
  valVar <- lift $ newTVar NoStaticValue
  -- TODO: condition value
  -- NoValue == no condition
  mkStaticProperty conditionEssence "close" Map.empty valVar StaticDiscoverRoot ActiveValueNonDiscoverable



doorStaticProperty :: CommonStaticProperties -> KBBuilder StaticProperty
doorStaticProperty csp@(CommonStaticProperties{posSProp, hpSProp}) = do
  openingCondSProp <- openingConditionStaticProperty csp
  closingCondSProp <- closingConditionStaticProperty csp

  openStateSProp   <- openStateStaticProperty   csp closingCondSProp
  closedStateSProp <- closedStateStaticProperty csp openingCondSProp

  -- Making a loop of states
  lift $ writeTVar (staticPropertyValueVar openingCondSProp)
       $ MaterializableStateValue "to open state"
       $ SharedInsterialization openStateSProp
  lift $ writeTVar (staticPropertyValueVar closingCondSProp)
       $ MaterializableStateValue "to closed state"
       $ SharedInsterialization closedStateSProp

  currentStateVar <- lift $ newTVar $ MaterializableStateValue "cur state" (SharedInsterialization closedStateSProp)

  let props = Map.fromList
        [ (statesPropType,    [ (SharedInsterialization openStateSProp)
                              , (SharedInsterialization closedStateSProp) ])
        , (inventoryPropType, [ (DirectMaterialization posSProp)
                              , (DirectMaterialization hpSProp) ])
        ]

  mkStaticProperty doorEssence "" props currentStateVar StaticDiscoverRoot ActiveValueDiscoverable
