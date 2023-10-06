{-# LANGUAGE TypeApplications #-}

module ZP.MaterializationSpec where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Materialization
import qualified ZP.Domain.Dynamic.Materialization as M
import ZP.Assets.KnowledgeBase.Essences
import ZP.Assets.KnowledgeBase.Common
import ZP.Assets.KnowledgeBase.Agents
import ZP.Assets.KnowledgeBase.Doors

import Test.Hspec

import Debug.Trace (trace)

import Data.Proxy
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.List as L



-- type Door = PropDict (EssRoot EDoor)
--   '[ PropKeyVal EHP (OwnProp (HPVal 100))
--    , PropKeyVal EPos (SharedProp (PosConst 3 5))        -- TODO: identified from the map

--     -- | Current state
--    , PropKeyVal EState (OwnProp OpenStateRef)

--     -- | Possible states
--    , PropKeyBag EStates
--       '[ OwnProp (StaticPropRef StateOpen)
--        , OwnProp (StaticPropRef StateClose)
--        ]

--     -- | Abilities to react to effects
--    , PropKeyBag EAbilities
--       '[ SharedProp (PropScript PushableScript)
--        ]
--    ]

data DynamicProperty = DynamicProperty
  { props :: Map.Map String ()
  }


door :: DynamicProperty
door = DynamicProperty
  (Map.fromList
    [ (M.mat (Proxy @EHP), ())

    ]
  )




spec :: Spec
spec = pure ()
  -- describe "AI test" $ do
  --   it "Selecting the next action. Should be observing (after noAct)" $ do
  --     idCounterVar <- newTVarIO 0
  --     worldVar     <- newTVarIO $ World Map.empty []
  --     stepVar      <- newTVarIO 0
  --     let rndSource = mkRndSource1 stepVar

  --     zpNet  <- atomically $ initZPNet1 idCounterVar rndSource worldVar
  --     atomically $ selectNextActionForObjName zpNet guard01Name

  --     verifyReport' zpNet guard01Name ["Action set: Essence \"observing\""]

  --   it "Evaluating the observing action" $ do
  --     idCounterVar <- newTVarIO 0
  --     worldVar     <- newTVarIO $ World Map.empty []
  --     stepVar      <- newTVarIO 0
  --     let rndSource = mkRndSource2 stepVar

  --     zpNet   <- atomically $ initZPNet1 idCounterVar rndSource worldVar
  --     atomically $ selectNextActionForObjName zpNet guard01Name
  --     atomically $ evaluateCurrentActionForObjName zpNet guard01Name

  --     verifyReport' zpNet guard01Name
  --       [ "Action set: Essence \"observing\""
  --       ]

  --   it "Evaluating the discovery action" $ do
  --     idCounterVar <- newTVarIO 0
  --     worldVar     <- newTVarIO $ World Map.empty []
  --     stepVar      <- newTVarIO 0
  --     let rndSource = mkRndSource2 stepVar

  --     zpNet <- atomically $ initZPNet1 idCounterVar rndSource worldVar

  --     -- outputGraph zpNet "zpnet.dot"

  --     actObj <- fromJust <$> (atomically $ getActingObject zpNet guard01Name)

  --     -- observing
  --     atomically $ selectNextAction actObj
  --     atomically $ evaluateCurrentAction zpNet actObj

  --     -- discovering
  --     atomically $ selectNextAction actObj
  --     atomically $ evaluateCurrentAction zpNet actObj

  --     verifyReport actObj
  --       [ "Action set: Essence \"observing\""
  --       , "Action set: Essence \"discovering\""
  --       , "discover: discovering acting object"
  --       , "discoverChunk, essence: Essence \"rat\", actObjId: ActivePropertyId 36, statPropId: StaticPropertyId 11"
  --       , "discoverPropertiesByTypesPropertyType \"actions\""
  --       , "discoverPropertiesByTypesPropertyType \"inventory\""
  --       , "discoverChunk, essence: Essence \"pos\", actObjId: ActivePropertyId 34, statPropId: StaticPropertyId 0"
  --       , "discoverChunk, essence: Essence \"hp\", actObjId: ActivePropertyId 35, statPropId: StaticPropertyId 1"
  --       , "discover': discoverPropety returned prop"
  --       , "discover: discovering acting object"
  --       , "discoverChunk, essence: Essence \"rat\", actObjId: ActivePropertyId 31, statPropId: StaticPropertyId 11"
  --       , "discoverPropertiesByTypesPropertyType \"actions\""
  --       , "discoverPropertiesByTypesPropertyType \"inventory\""
  --       , "discoverChunk, essence: Essence \"pos\", actObjId: ActivePropertyId 29, statPropId: StaticPropertyId 0"
  --       , "discoverChunk, essence: Essence \"hp\", actObjId: ActivePropertyId 30, statPropId: StaticPropertyId 1"
  --       , "discover': discoverPropety returned prop"
  --       , "discover: discovering self not needed."
  --       ]

  --   it "Actions rotation logic test" $ do
  --     idCounterVar <- newTVarIO 0
  --     worldVar     <- newTVarIO $ World Map.empty []
  --     stepVar      <- newTVarIO 0
  --     let rndSource = mkRndSource2 stepVar
  --     zpNet <- atomically $ initZPNet1 idCounterVar rndSource worldVar

  --     atomically $ selectNextActionForObjName zpNet guard01Name
  --     atomically $ selectNextActionForObjName zpNet guard01Name
  --     atomically $ selectNextActionForObjName zpNet guard01Name
  --     atomically $ selectNextActionForObjName zpNet guard01Name
  --     atomically $ selectNextActionForObjName zpNet guard01Name
  --     atomically $ selectNextActionForObjName zpNet guard01Name

  --     verifyReport' zpNet guard01Name
  --       [ "Action set: Essence \"observing\""
  --       , "Action set: Essence \"discovering\""
  --       , "Action set: Essence \"setting goals\""
  --       , "Action set: Essence \"planning\""
  --       , "Action set: Essence \"following a plan\""
  --       , "Action set: Essence \"observing\""
  --       ]

  --   xit "Evaluating the goals setting action" $ do
  --     idCounterVar <- newTVarIO 0
  --     worldVar     <- newTVarIO $ World Map.empty []
  --     stepVar      <- newTVarIO 0
  --     let rndSource = mkRndSource2 stepVar

  --     zpNet <- atomically $ initZPNet1 idCounterVar rndSource worldVar

  --     actObj <- fromJust <$> (atomically $ getActingObject zpNet guard01Name)

  --     -- observing
  --     atomically $ selectNextAction actObj
  --     atomically $ evaluateCurrentAction zpNet actObj

  --     -- discovering
  --     atomically $ selectNextAction actObj
  --     atomically $ evaluateCurrentAction zpNet actObj

  --     clearReporter actObj
  --     clearGlobalReporter zpNet

  --     -- setting goals
  --     atomically $ selectNextAction actObj
  --     atomically $ evaluateCurrentAction zpNet actObj

  --     verifyReport actObj []

  --   it "Switching door states" $ do
  --     idCounterVar <- newTVarIO 0
  --     worldVar     <- newTVarIO $ World Map.empty []
  --     stepVar      <- newTVarIO 0
  --     let rndSource = mkRndSource2 stepVar

  --     zpNet <- atomically $ initZPNet2 idCounterVar rndSource worldVar
  --     actObj <- fromJust <$> (atomically $ getActingObject zpNet door01)

  --     -- outputGraph zpNet "doors1.dot"

  --     allActivations <- atomically $ getAllActivations actObj
  --     Map.size allActivations `shouldBe` 2

  --     currentActivations <- atomically $ getCurrentActivations actObj
  --     Map.size currentActivations `shouldBe` 1

  --     mbNewStateProp <- atomically
  --       $ activate zpNet actObj
  --       $ fst $ Map.findMin currentActivations

  --     -- outputGraph zpNet "doors2.dot"

  --     mbCurStateProp <- atomically $ getCurrentStateProperty actObj
  --     case (mbNewStateProp, mbCurStateProp) of
  --       (Just prop1, Just prop2) -> activePropertyId prop1 `shouldBe` activePropertyId prop2
  --       (_, Nothing) -> fail "Invalid activation2"
  --       (Nothing, _) -> fail "Invalid activation1"

  --     verifyReport actObj []
  --     verifyGlobalReport zpNet []
