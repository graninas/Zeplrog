{-# LANGUAGE DataKinds #-}

module ZP.MaterializationSpec where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Dynamic.Model.Common as DMod
import qualified ZP.Domain.Dynamic.Model.Property as DMod
import qualified ZP.Domain.Browser.Language as Browser
import qualified ZP.Domain.Browser.Methods as Browser
import qualified ZP.Domain.Browser.Implementation as Browser
import ZP.Domain.Dynamic.Model.Common
import ZP.Domain.Dynamic.Model.Property
import ZP.Domain.Dynamic.Materialization
import qualified ZP.Assets.KnowledgeBase as KB

import Test.Hspec

import Debug.Trace (trace)

import Data.Proxy
import qualified Data.Map as Map


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

type HPValOwnProp     = SMod.OwnProp (KB.HPVal 100)
type PosValSharedProp = SMod.SharedProp (KB.PosConst 3 5)
type DoorStateProp    = SMod.OwnProp KB.OpenStateRef


matDoor :: Materializer DynamicProperty
matDoor = mat False $ Proxy @KB.Door


-- Manually materialized door

matDoorCustom :: Materializer DynamicProperty
matDoorCustom = do
  ess <- mat False $ Proxy @(SMod.EssRoot KB.EDoor)

  ehp       <- mat False $ Proxy @KB.EHP
  hpPropOwn <- mat False $ Proxy @HPValOwnProp

  ePos          <- mat False $ Proxy @KB.EPos
  posPropShared <- mat False $ Proxy @PosValSharedProp

  -- eState   <- mat False $ Proxy @KB.EState
  -- stateOwn <- mat False $ Proxy @DoorStateProp

  propsMapVar <- liftIO $ newTVarIO $
    Map.fromList
      [ (ehp,    hpPropOwn)
      , (ePos,   posPropShared)
      -- , (eState, stateOwn)
      ]

  dynValVar <- liftIO $ newTVarIO Nothing

  let staticProp = Proxy @KB.Door
  let staticPropRef = StaticPropRef staticProp
  pure $ DynamicProperty ess staticPropRef propsMapVar dynValVar


spec :: Spec
spec = do
  describe "Materialization test" $ do
    it "Materialization: value prop" $ do
      (_, DynamicProperty _ _ propsVar valVar) <- runMaterializer matDoorCustom

      mbVal <- readTVarIO valVar
      case mbVal of
        Nothing -> pure ()
        Just _  -> error "Unexpected value"

      ehp <- mat' $ Proxy @KB.EHP

      props <- readTVarIO propsVar
      case Map.lookup ehp props of
        Nothing -> error "Unexpected nothing"
        Just (OwnDynamicProperty (DynamicProperty _ _ _ valVar2)) -> do
          mbVal <- readTVarIO valVar2
          case mbVal of
            Nothing -> error "Unexpected no value"
            Just (VarValue valVar3) -> do
              val3 <- readTVarIO valVar3
              val3 `shouldBe` IntValue 100
            Just _ -> error "Invalid val"
        Just _ -> error "Invalid dyn prop"

    it "Materialization: 1 shared prop" $ do
      (Env spsVar, _) <- runMaterializer matDoorCustom
      sps <- readTVarIO spsVar
      -- TODO: verify the prop itself
      Map.size sps `shouldBe` 1

    it "Full materialization" $ do
      (Env spsVar, _) <- runMaterializer matDoor
      sps <- readTVarIO spsVar
      -- TODO: verify the prop itself
      Map.size sps `shouldBe` 1

  describe "Static properties test" $ do
    it "Static prop is accessible from a dynamic prop 1" $ do
      (_, DynamicProperty _ statPropRef _ _) <- runMaterializer matDoorCustom
      let ess = Browser.browseDyn Browser.GetEssence statPropRef
      ess `shouldBe` "object:door"

    it "Static prop is accessible from a dynamic prop 2" $ do
      (_, DynamicProperty _ _ propsVar _) <- runMaterializer matDoorCustom
      props <- readTVarIO propsVar
      ehp <- mat' $ Proxy @KB.EHP
      case Map.lookup ehp props of
        Nothing -> error "Unexpected nothing"
        Just (OwnDynamicProperty (DynamicProperty _ statPropRef _ _)) -> do
          let ess = Browser.browseDyn Browser.GetEssence statPropRef
          ess `shouldBe` "intrinsics:hp"

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
