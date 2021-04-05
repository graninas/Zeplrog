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

  , obsrvingSProp     :: StaticProperty
  , settingGoalsSProp :: StaticProperty
  , planningSProp     :: StaticProperty
  }


--------------------------------------------------------------------------------

mkCommonStaticProperties :: IdCounter -> TVar Essences -> STM CommonStaticProperties
mkCommonStaticProperties idCounterVar essencesVar =
  CommonStaticProperties
    <$> mkStaticProperty idCounterVar essencesVar posEssence      Map.empty StaticDiscoverLeaf ActiveDiscoverable
    <*> mkStaticProperty idCounterVar essencesVar hpEssence       Map.empty StaticDiscoverLeaf ActiveNonDiscoverable
    <*> mkStaticProperty idCounterVar essencesVar fireWandEssence Map.empty StaticNonDiscoverable ActiveNonDiscoverable
    <*> mkStaticProperty idCounterVar essencesVar iceWandEssence  Map.empty StaticNonDiscoverable ActiveNonDiscoverable

    <*> mkStaticProperty idCounterVar essencesVar observingEssence    Map.empty StaticNonDiscoverable ActiveNonDiscoverable
    <*> mkStaticProperty idCounterVar essencesVar settingGoalsEssence Map.empty StaticNonDiscoverable ActiveNonDiscoverable
    <*> mkStaticProperty idCounterVar essencesVar planningEssence     Map.empty StaticNonDiscoverable ActiveNonDiscoverable

dogStaticProperty :: IdCounter -> TVar Essences -> CommonStaticProperties -> STM StaticProperty
dogStaticProperty idCounterVar essencesVar CommonStaticProperties{..} = do
  let props = Map.fromList
        [ (inventoryPropType, [ posSProp, hpSProp ])
        ]
  mkStaticProperty idCounterVar essencesVar dogEssence props StaticDiscoverRoot ActiveNonDiscoverable

guardStaticProperty :: IdCounter -> TVar Essences -> CommonStaticProperties -> STM StaticProperty
guardStaticProperty idCounterVar essencesVar CommonStaticProperties{..} = do
  -- TODO: add a new type of a dynamic discoverability: discoverability on usage
  let props = Map.fromList
        [ (inventoryPropType, [ posSProp, hpSProp, fireWandSProp, iceWandSProp ])
        , (actionPropType, [obsrvingSProp, settingGoalsSProp, planningSProp])
        ]
  mkStaticProperty idCounterVar essencesVar guardEssence props StaticDiscoverRoot ActiveNonDiscoverable

initKnowledgeBase :: IdCounter -> STM KnowledgeBase
initKnowledgeBase idCounterVar = do
  essencesVar <- newTVar Map.empty
  commonStatProps <- mkCommonStaticProperties idCounterVar essencesVar
  statProps <- sequence
    [ guardStaticProperty idCounterVar essencesVar commonStatProps
    , dogStaticProperty   idCounterVar essencesVar commonStatProps
    ]
  essences <- readTVar essencesVar
  pure $ KnowledgeBase statProps essences

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
