{-# LANGUAGE DuplicateRecordFields #-}

module ZP.TestData.KnowledgeBase.Common where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic
import ZP.AI.Types
import ZP.AI.StaticKnowledge
import ZP.TestData.KnowledgeBase.Essences

import qualified Data.Map as Map
import qualified Data.Set as Set

data KBBuilderEnv = KBBuilderEnv
  { idCounterVar :: IdCounter
  , essencesVar :: TVar Essences
  }

type KBBuilder a = ReaderT KBBuilderEnv STM a


data CommonStaticProperties = CommonStaticProperties
  { posSProp      :: StaticProperty
  , hpSProp       :: StaticProperty
  , fireWandSProp :: StaticProperty
  , iceWandSProp  :: StaticProperty

  , obsrvingSProp      :: StaticProperty
  , discoveringSProp   :: StaticProperty
  , settingGoalsSProp  :: StaticProperty
  , planningSProp      :: StaticProperty
  , followingPlanSProp :: StaticProperty
  , noActionSProp      :: StaticProperty

  , goalSProp :: StaticProperty
  }


mkStaticProperty
  :: Essence
  -> StaticPropertyMap
  -> TVar StaticPropertyValue
  -> StaticPropertyDiscoverability
  -> ActiveValueDiscoverability
  -> KBBuilder StaticProperty
mkStaticProperty ess props valVar statDisc actDisc = do
  KBBuilderEnv idCounterVar essencesVar <- ask
  propId <- lift $ getStaticPropertyId idCounterVar
  let prop = StaticProperty propId ess props valVar statDisc actDisc
  lift $ modifyTVar' essencesVar $ Map.insert ess prop
  pure prop

mkCommonStaticProperty
  :: Essence
  -> StaticPropertyDiscoverability
  -> ActiveValueDiscoverability
  -> KBBuilder StaticProperty
mkCommonStaticProperty essence statDisc actDisc = do
  valVar <- lift $ newTVar NoStaticValue
  mkStaticProperty essence Map.empty valVar statDisc actDisc

mkCommonStaticProperties :: KBBuilder CommonStaticProperties
mkCommonStaticProperties = do

  CommonStaticProperties
    <$> mkCommonStaticProperty posEssence      StaticDiscoverLeaf    ActiveValueDiscoverable
    <*> mkCommonStaticProperty hpEssence       StaticDiscoverLeaf    ActiveValueNonDiscoverable
    <*> mkCommonStaticProperty fireWandEssence StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkCommonStaticProperty iceWandEssence  StaticNonDiscoverable ActiveValueNonDiscoverable

    <*> mkCommonStaticProperty observingEssence     StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkCommonStaticProperty discoveringEssence   StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkCommonStaticProperty settingGoalsEssence  StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkCommonStaticProperty planningEssence      StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkCommonStaticProperty followingPlanEssence StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkCommonStaticProperty noActionEssence      StaticNonDiscoverable ActiveValueNonDiscoverable

    <*> mkCommonStaticProperty goalEssence StaticNonDiscoverable ActiveValueNonDiscoverable


killGoalStaticProperty :: StaticProperty -> KBBuilder StaticProperty
killGoalStaticProperty targetSProp = do
  valVar <- lift $ newTVar NoStaticValue   -- (TargetValue (StaticPropertyValue targetSProp))
  mkStaticProperty
    goalEssence           --- not unique??? (used in common stat props)
    Map.empty
    valVar
    StaticNonDiscoverable
    ActiveValueNonDiscoverable


commonActionsSProps :: CommonStaticProperties -> [ StaticProperty ]
commonActionsSProps CommonStaticProperties {..} =
  [ noActionSProp
  , obsrvingSProp
  , discoveringSProp
  , settingGoalsSProp
  , planningSProp
  , followingPlanSProp
  ]
