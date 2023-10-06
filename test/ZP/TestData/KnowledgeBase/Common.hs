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
  -> StaticPropertyDiscoverability
  -> ActiveValueDiscoverability
  -> KBBuilder StaticProperty
mkStaticProperty essence props statDisc actDisc = do
  KBBuilderEnv idCounterVar essencesVar <- ask
  propId <- lift $ getStaticPropertyId idCounterVar
  let prop = StaticProperty propId essence props statDisc actDisc
  lift $ modifyTVar' essencesVar $ Map.insert essence prop
  pure prop

mkCommonStaticProperties :: KBBuilder CommonStaticProperties
mkCommonStaticProperties =
  CommonStaticProperties
    <$> mkStaticProperty posEssence      Map.empty StaticDiscoverLeaf    ActiveValueDiscoverable
    <*> mkStaticProperty hpEssence       Map.empty StaticDiscoverLeaf    ActiveValueNonDiscoverable
    <*> mkStaticProperty fireWandEssence Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkStaticProperty iceWandEssence  Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable

    <*> mkStaticProperty observingEssence     Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkStaticProperty discoveringEssence   Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkStaticProperty settingGoalsEssence  Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkStaticProperty planningEssence      Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkStaticProperty followingPlanEssence Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkStaticProperty noActionEssence      Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable

    <*> mkStaticProperty goalEssence Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable


killGoalStaticProperty :: StaticProperty -> KBBuilder StaticProperty
killGoalStaticProperty targetSProp =
  mkStaticProperty
    goalEssence
    (Map.singleton targetPropType [targetSProp])            -- TODO: need different types of goals
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
