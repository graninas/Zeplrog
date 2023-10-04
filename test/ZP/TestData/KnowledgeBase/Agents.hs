{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module ZP.TestData.KnowledgeBase.Agents where

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








ratStaticProperty :: CommonStaticProperties -> KBBuilder StaticProperty
ratStaticProperty CommonStaticProperties{..} = do
  valVar <- lift $ newTVar NoStaticValue
  let props = Map.fromList
        [ (inventoryPropType, [ DirectMaterialization posSProp
                              , DirectMaterialization hpSProp ])
        , (actionsPropType,   [ DirectMaterialization noActionSProp ])              -- this rat doesn't do anything...
        ]
  mkStaticProperty ratEssence "" props valVar StaticDiscoverRoot ActiveValueNonDiscoverable



guardStaticProperty
  :: StaticProperty
  -> CommonStaticProperties
  -> KBBuilder StaticProperty
guardStaticProperty ratSProp commonSProps@(CommonStaticProperties{..}) = do
  valVar <- lift $ newTVar NoStaticValue
  -- TODO: add a new type of a dynamic discoverability: discoverability on usage
  killRatGoalSProp <- killGoalStaticProperty ratSProp
  let props = Map.fromList
        [ (inventoryPropType, [ DirectMaterialization posSProp
                              , DirectMaterialization hpSProp
                              , DirectMaterialization fireWandSProp
                              , DirectMaterialization iceWandSProp ])
        , (actionsPropType, map DirectMaterialization $ commonActionsSProps commonSProps)
        , (goalsPropType,     [ DirectMaterialization killRatGoalSProp ])
        ]
  mkStaticProperty guardEssence "" props valVar StaticDiscoverRoot ActiveValueNonDiscoverable



------------------

-- mkAbstractKillRatGoal :: IdCounter -> StaticProperty -> STM ActiveProperty
-- mkAbstractKillRatGoal idCounterVar ratStatProp = do
--   propId <- getActivePropertyId idCounterVar
--   propsVar <- newTVar Map.empty
--   pure $ ActiveProperty propId abstractGoalEssence (Just ratStatProp) propsVar


-- guardActingObject :: IdCounter -> StaticProperty -> STM (ActingObjectId, ActingObject)
-- guardActingObject idCounterVar ratSProp = do
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
--   killRatGoal <- mkAbstractKillRatGoal idCounterVar ratSProp
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
--   goalsVar <- newTVar [killRatGoal]
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
