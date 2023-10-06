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
  valVar <- lift $ newTVar NoStaticValue
  -- TODO: passable property & discoverability
  mkStaticProperty openEssence Map.empty valVar StaticDiscoverRoot ActiveValueNonDiscoverable


closedStateStaticProperty :: CommonStaticProperties -> KBBuilder StaticProperty
closedStateStaticProperty CommonStaticProperties{..} = do
  valVar <- lift $ newTVar NoStaticValue
  -- TODO: passable property & discoverability
  mkStaticProperty closedEssence Map.empty valVar StaticDiscoverRoot ActiveValueNonDiscoverable


openingConditionStaticProperty :: CommonStaticProperties -> StaticProperty -> KBBuilder StaticProperty
openingConditionStaticProperty csp stateSProp = do
  valVar <- lift $ newTVar NoStaticValue
  let props = Map.singleton targetPropType [(DirectMaterialization stateSProp)]
  -- TODO: condition value
  -- NoValue == no condition
  mkStaticProperty conditionEssence props valVar StaticDiscoverRoot ActiveValueNonDiscoverable

closingConditionStaticProperty :: CommonStaticProperties -> StaticProperty -> KBBuilder StaticProperty
closingConditionStaticProperty csp stateSProp = do
  valVar <- lift $ newTVar NoStaticValue
  let props = Map.singleton targetPropType [(DirectMaterialization stateSProp)]
  -- TODO: condition value
  -- NoValue == no condition
  mkStaticProperty conditionEssence props valVar StaticDiscoverRoot ActiveValueNonDiscoverable



doorStaticProperty :: CommonStaticProperties -> KBBuilder StaticProperty
doorStaticProperty csp@(CommonStaticProperties{posSProp, hpSProp}) = do
  openStateSProp   <- openStateStaticProperty csp
  closedStateSProp <- closedStateStaticProperty csp

  openingCondSProp <- openingConditionStaticProperty csp openStateSProp
  closingCondSProp <- closingConditionStaticProperty csp closedStateSProp

  -- Making a loop of states
  lift $ writeTVar (staticPropertyValueVar openStateSProp)
       $ MaterializableStateValue "close"
       $ SharedMaterialization closingCondSProp
  lift $ writeTVar (staticPropertyValueVar closedStateSProp)
       $ MaterializableStateValue "open"
       $ SharedMaterialization openingCondSProp

  currentStateVar <- lift $ newTVar $ MaterializableStateValue "cur state" (SharedMaterialization closedStateSProp)

  let props = Map.fromList
        [ (statesPropType,    [ (SharedMaterialization openingCondSProp)
                              , (SharedMaterialization closingCondSProp) ])
        , (inventoryPropType, [ (DirectMaterialization posSProp)
                              , (DirectMaterialization hpSProp) ])
        ]

  mkStaticProperty doorEssence props currentStateVar StaticDiscoverRoot ActiveValueDiscoverable
