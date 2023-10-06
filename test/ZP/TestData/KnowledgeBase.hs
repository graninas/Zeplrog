{-# LANGUAGE DuplicateRecordFields #-}

module ZP.TestData.KnowledgeBase where

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
dogEssence      = Essence "dog"
guardEssence    = Essence "guard"

-- -----------------------------------------------------------------------------

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


--------------------------------------------------------------------------------

mkCommonStaticProperties :: IdCounter -> TVar Essences -> STM CommonStaticProperties
mkCommonStaticProperties idCounterVar essencesVar =
  CommonStaticProperties
    <$> mkStaticProperty idCounterVar essencesVar posEssence      Map.empty StaticDiscoverLeaf    ActiveValueDiscoverable
    <*> mkStaticProperty idCounterVar essencesVar hpEssence       Map.empty StaticDiscoverLeaf    ActiveValueNonDiscoverable
    <*> mkStaticProperty idCounterVar essencesVar fireWandEssence Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkStaticProperty idCounterVar essencesVar iceWandEssence  Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable

    <*> mkStaticProperty idCounterVar essencesVar observingEssence     Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkStaticProperty idCounterVar essencesVar discoveringEssence   Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkStaticProperty idCounterVar essencesVar settingGoalsEssence  Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkStaticProperty idCounterVar essencesVar planningEssence      Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkStaticProperty idCounterVar essencesVar followingPlanEssence Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable
    <*> mkStaticProperty idCounterVar essencesVar noActionEssence      Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable

    <*> mkStaticProperty idCounterVar essencesVar goalEssence Map.empty StaticNonDiscoverable ActiveValueNonDiscoverable

killGoalStaticProperty :: IdCounter -> TVar Essences -> StaticProperty -> STM StaticProperty
killGoalStaticProperty idCounterVar essencesVar targetSProp =
  mkStaticProperty idCounterVar essencesVar goalEssence
    (Map.singleton killTargetPropType [targetSProp])            -- TODO: type of goal should be explicit
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

dogStaticProperty :: IdCounter -> TVar Essences -> CommonStaticProperties -> STM StaticProperty
dogStaticProperty idCounterVar essencesVar CommonStaticProperties{..} = do
  let props = Map.fromList
        [ (inventoryPropType, [ posSProp, hpSProp ])
        , (actionsPropType,   [ noActionSProp ])              -- this dog doesn't do anything...
        ]
  mkStaticProperty idCounterVar essencesVar dogEssence props StaticDiscoverRoot ActiveValueNonDiscoverable

guardStaticProperty
  :: IdCounter
  -> TVar Essences
  -> StaticProperty
  -> CommonStaticProperties
  -> STM StaticProperty
guardStaticProperty idCounterVar essencesVar dogSProp commonSProps@(CommonStaticProperties{..}) = do
  -- TODO: add a new type of a dynamic discoverability: discoverability on usage
  killDogGoalSProp <- killGoalStaticProperty idCounterVar essencesVar dogSProp
  let props = Map.fromList
        [ (inventoryPropType, [ posSProp, hpSProp, fireWandSProp, iceWandSProp ])
        , (actionsPropType,    commonActionsSProps commonSProps)
        , (goalsPropType,     [ killDogGoalSProp ])
        ]
  mkStaticProperty idCounterVar essencesVar guardEssence props StaticDiscoverRoot ActiveValueNonDiscoverable

initKnowledgeBase :: IdCounter -> STM (KnowledgeBase, CommonStaticProperties)
initKnowledgeBase idCounterVar = do
  essencesVar <- newTVar Map.empty
  commonStatProps <- mkCommonStaticProperties idCounterVar essencesVar
  dogSProp <- dogStaticProperty   idCounterVar essencesVar commonStatProps
  statProps <- sequence
    [ guardStaticProperty idCounterVar essencesVar dogSProp commonStatProps
    ]
  essences <- readTVar essencesVar
  pure (KnowledgeBase statProps essences, commonStatProps)

------------------

-- mkAbstractKillDogGoal :: IdCounter -> StaticProperty -> STM ActiveProperty
-- mkAbstractKillDogGoal idCounterVar dogStatProp = do
--   propId <- getActivePropertyId idCounterVar
--   propsVar <- newTVar Map.empty
--   pure $ ActiveProperty propId abstractGoalEssence (Just dogStatProp) propsVar


-- guardActingObject :: IdCounter -> StaticProperty -> STM (ActingObjectId, ActingObject)
-- guardActingObject idCounterVar dogSProp = do
--
--   posProp       <- mkProperty posEssence      idCounterVar
--   hpProp        <- mkProperty hpEssence       idCounterVar
--   fireWandProp  <- mkProperty fireWandEssence idCounterVar
--   iceWandProp   <- mkProperty iceWandEssence  idCounterVar
--
--   observeAct  <- mkProperty observingEssence    idCounterVar
--   setGoalsAct <- mkProperty settingGoalsEssence idCounterVar
--   planAct     <- mkProperty planningEssence     idCounterVar
--
--   killDogGoal <- mkAbstractKillDogGoal idCounterVar dogSProp
--
--   invVar <- newTVar
--       [ posProp
--       , hpProp
--       , fireWandProp
--       , iceWandProp
--       ]
--
--   actsVar <- newTVar
--     [ observeAct
--     , setGoalsAct
--     , planAct
--     ]
--
--   goalsVar <- newTVar [killDogGoal]
--
--   rootPropsVar <- newTVar $ Map.fromList
--     [ (inventoryPropType, invVar)
--     , (actionPropType, actsVar)
--     , (abstractGoalPropType, goalsVar)
--     ]
--
--   curActVar <- newTVar Nothing
--
--   rootPropId <- getActivePropertyId idCounterVar
--   actObjId   <- getActingObjectId idCounterVar
--   let rootProp = ActiveProperty rootPropId guardEssence Nothing rootPropsVar
--   knownObjsVar <- newTVar Map.empty
--   let actObj = ActingObject (ActingObjectName "guard 01") actObjId rootProp curActVar knownObjsVar
--   pure
--     -- $ traceShow actObjId
--     $ (actObjId, actObj)
--
