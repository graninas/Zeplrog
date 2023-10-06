{-# LANGUAGE DuplicateRecordFields #-}

module ZP.AISpec where

import ZP.Prelude
import ZP.Types
import ZP.App
import ZP.Game.Types
import ZP.Game.Logic
import ZP.AI.Types
import ZP.AI.Materialization
import ZP.AI.Logic
import ZP.AI.StaticKnowledge
import ZP.TestData.KnowledgeBase
import ZP.TestData.GraphBuilder

import Test.Hspec

import Debug.Trace (trace)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.List as L

type Report = [String]

materializeActiveObject
  :: IdCounter
  -> KnowledgeBase
  -> ActiveProperty
  -> ActingObjectName
  -> Essence
  -> PropertiesSetter
  -> STM (Maybe ActingObject)
materializeActiveObject idCounterVar kb@(KnowledgeBase {essences}) noActProp name ess propsSetter = do
  case Map.lookup ess essences of
    Nothing ->
      trace ("materializeActiveObject: nothing to materialize, static property essence not found: " <> show ess) $ pure Nothing
    Just sProp -> do
      matPropsVar <- newTVar Map.empty

      rootProp  <- materializeLink idCounterVar kb matPropsVar propsSetter $ DirectMaterialization sProp
      actProps  <- getPropertiesOfType rootProp actionsPropType

      let f' actProp = (essence $ staticProperty actProp, actProp)
      actsByEssenseVar <- newTVar $ Map.fromList $ map f' actProps

      curActVar <- newTVar $ case actProps of
        []    -> noActProp
        (p:_) -> p
      knownObjsVar <- newTVar Map.empty
      actObjId     <- getActingObjectId idCounterVar
      reporter     <- newTVar [] >>= pure . Just
      pure $ Just $ ActingObject name actObjId rootProp curActVar actsByEssenseVar knownObjsVar reporter


guard01Name :: ActingObjectName
guard01Name = ActingObjectName "guard 01"

rat01Name   :: ActingObjectName
rat01Name   = ActingObjectName "rat 01"

rat02Name   :: ActingObjectName
rat02Name   = ActingObjectName "rat 02"

door01   = ActingObjectName "door 01"


{- Actions loop
Actions point to each other via the essence. Next action's essence should be placed
into a property value, specifically into PairValue as the first argument:

PairValue (EssenceValue discoveringEssence) NoValue     -- next is discovering action

The second member of a pair is used for input data for that action.
This input data can be set not only by evaluating an action by also by external actors and events.

Actions may rotate in a loop.

The noActionsEssence is used as entry point into the loop.

-}

commonActionsLoop :: [(Essence, PropertyValue)]
commonActionsLoop =
  [ (noActionEssence,      PairValue (EssenceValue "" observingEssence)     NoValue)  -- starting point
  , (observingEssence,     PairValue (EssenceValue "" discoveringEssence)   NoValue)  -- no input params yet
  , (discoveringEssence,   PairValue (EssenceValue "" settingGoalsEssence)  NoValue)  -- no input params yet
  , (settingGoalsEssence,  PairValue (EssenceValue "" planningEssence)      NoValue)  -- no input params yet
  , (planningEssence,      PairValue (EssenceValue "" followingPlanEssence) NoValue)  -- no input params yet
  , (followingPlanEssence, PairValue (EssenceValue "" observingEssence)     NoValue)  -- no input params yet
  ]

initActiveObjects1
  :: IdCounter
  -> (KnowledgeBase, CommonStaticProperties)
  -> STM (Map ActingObjectId ActingObject)
initActiveObjects1 idCounterVar (kb, commonSProps) = do
  let guardProps = Map.fromList $
        [ (posEssence, PositionValue (3, 3))
        , (hpEssence, IntValue 100)
        ] <> commonActionsLoop
  let rat01Props = Map.fromList
        [ (posEssence, PositionValue (3, 5))
        , (hpEssence, IntValue 100)
        ]
  let rat02Props = Map.fromList
        [ (posEssence, PositionValue (3, 4))
        , (hpEssence, IntValue 100)
        ]

  matPropsVar <- newTVar Map.empty
  noActProp <- materializeLink idCounterVar kb matPropsVar Map.empty
    $ DirectMaterialization $ noActionSProp commonSProps

  mbObjs <- sequence
    [ materializeActiveObject idCounterVar kb noActProp guard01Name guardEssence guardProps
    , materializeActiveObject idCounterVar kb noActProp rat01Name   ratEssence   rat01Props
    , materializeActiveObject idCounterVar kb noActProp rat02Name   ratEssence   rat02Props
    ]
  pure $ Map.fromList $ map (\obj -> (actingObjectId obj, obj)) $ catMaybes mbObjs


initActiveObjects2
  :: IdCounter
  -> (KnowledgeBase, CommonStaticProperties)
  -> STM (Map ActingObjectId ActingObject)
initActiveObjects2 idCounterVar (kb, commonSProps) = do
  let door01Props = Map.fromList
        [ (posEssence, PositionValue (2, 3))
        , (hpEssence, IntValue 100)
        ]

  matPropsVar <- newTVar Map.empty
  noActProp <- materializeLink idCounterVar kb matPropsVar Map.empty
    $ DirectMaterialization $ noActionSProp commonSProps

  mbObjs <- sequence
    [ materializeActiveObject idCounterVar kb noActProp door01 doorEssence Map.empty
    ]
  pure $ Map.fromList $ map (\obj -> (actingObjectId obj, obj)) $ catMaybes mbObjs


initZPNet1
  :: IdCounter
  -> RndSource
  -> TVar World
  -> STM ZPNet
initZPNet1 idCounterVar rndSource worldVar = do
  kb'@(kb, _) <- initKnowledgeBase1 idCounterVar
  actObjs     <- initActiveObjects1 idCounterVar kb'
  reporter    <- newTVar [] >>= pure . Just
  let actObjsByName = Map.fromList $ map (\(_, o) -> (actingObjectName o, o)) $ Map.toList actObjs
  pure $ ZPNet kb actObjs actObjsByName rndSource worldVar reporter

initZPNet2
  :: IdCounter
  -> RndSource
  -> TVar World
  -> STM ZPNet
initZPNet2 idCounterVar rndSource worldVar = do
  kb'@(kb, _) <- initKnowledgeBase2 idCounterVar
  actObjs     <- initActiveObjects2 idCounterVar kb'
  reporter    <- newTVar [] >>= pure . Just
  let actObjsByName = Map.fromList $ map (\(_, o) -> (actingObjectName o, o)) $ Map.toList actObjs
  pure $ ZPNet kb actObjs actObjsByName rndSource worldVar reporter


mkRndSource1 :: TVar Int -> Int -> STM Int
mkRndSource1 stepVar input | input <= 0 = pure 0
mkRndSource1 stepVar input = do
  step <- readTVar stepVar
  modifyTVar' stepVar (+1)
  pure $ case step of
    _ -> 0

mkRndSource2 :: TVar Int -> Int -> STM Int
mkRndSource2 stepVar input | input <= 0 = pure 0
mkRndSource2 stepVar input = do
  step <- readTVar stepVar
  modifyTVar' stepVar (+1)
  pure $ case step of
    0 -> 0
    1 -> 0
    _ -> 0


clearReporter :: ActingObject -> IO ()
clearReporter ActingObject {actingObjectReporter} =
  case actingObjectReporter of
    Nothing -> pure ()
    Just reporterVar -> atomically $ writeTVar reporterVar []

clearGlobalReporter :: ZPNet -> IO ()
clearGlobalReporter ZPNet {zpNetReporter} =
  case zpNetReporter of
    Nothing -> pure ()
    Just reporterVar -> atomically $ writeTVar reporterVar []

verifyReport :: ActingObject -> Report -> IO ()
verifyReport ActingObject {actingObjectReporter} expectedReport =
  case actingObjectReporter of
    Nothing -> pure ()
    Just reporterVar -> do
      report <- atomically $ do
        rep <- readTVar reporterVar
        writeTVar reporterVar []
        pure rep
      reverse report `shouldBe` expectedReport

verifyReport' :: ZPNet -> ActingObjectName -> Report -> IO ()
verifyReport' zpNet name expectedReport = do
  mbActObj <- atomically $ getActingObject zpNet name
  case mbActObj of
    Nothing  -> fail "Acting object not found."
    Just obj -> verifyReport obj expectedReport

verifyGlobalReport :: ZPNet -> Report -> IO ()
verifyGlobalReport ZPNet {zpNetReporter} expectedReport =
  case zpNetReporter of
    Nothing -> pure ()
    Just reporterVar -> do
      report <- atomically $ do
        rep <- readTVar reporterVar
        writeTVar reporterVar []
        pure rep
      reverse report `shouldBe` expectedReport

outputGraph :: ZPNet -> String -> IO ()
outputGraph zpNet fileName = do
  graphLines <- atomically $ buildGraph zpNet
  writeFile ("./test/test_data/graphs/" <> fileName) $ T.pack $ L.intercalate "\n" graphLines


spec :: Spec
spec =
  describe "AI test" $ do
    it "Selecting the next action. Should be observing (after noAct)" $ do
      idCounterVar <- newTVarIO 0
      worldVar     <- newTVarIO $ World Map.empty []
      stepVar      <- newTVarIO 0
      let rndSource = mkRndSource1 stepVar

      zpNet  <- atomically $ initZPNet1 idCounterVar rndSource worldVar
      atomically $ selectNextActionForObjName zpNet guard01Name

      verifyReport' zpNet guard01Name ["Action set: Essence \"observing\""]

    it "Evaluating the observing action" $ do
      idCounterVar <- newTVarIO 0
      worldVar     <- newTVarIO $ World Map.empty []
      stepVar      <- newTVarIO 0
      let rndSource = mkRndSource2 stepVar

      zpNet   <- atomically $ initZPNet1 idCounterVar rndSource worldVar
      atomically $ selectNextActionForObjName zpNet guard01Name
      atomically $ evaluateCurrentActionForObjName zpNet guard01Name

      verifyReport' zpNet guard01Name
        [ "Action set: Essence \"observing\""
        ]

    it "Evaluating the discovery action" $ do
      idCounterVar <- newTVarIO 0
      worldVar     <- newTVarIO $ World Map.empty []
      stepVar      <- newTVarIO 0
      let rndSource = mkRndSource2 stepVar

      zpNet <- atomically $ initZPNet1 idCounterVar rndSource worldVar

      -- outputGraph zpNet "zpnet.dot"

      actObj <- fromJust <$> (atomically $ getActingObject zpNet guard01Name)

      -- observing
      atomically $ selectNextAction actObj
      atomically $ evaluateCurrentAction zpNet actObj

      -- discovering
      atomically $ selectNextAction actObj
      atomically $ evaluateCurrentAction zpNet actObj

      verifyReport actObj
        [ "Action set: Essence \"observing\""
        , "Action set: Essence \"discovering\""
        , "discover: discovering acting object"
        , "discoverChunk, essence: Essence \"rat\", actObjId: ActivePropertyId 36, statPropId: StaticPropertyId 11"
        , "discoverPropertiesByTypesPropertyType \"actions\""
        , "discoverPropertiesByTypesPropertyType \"inventory\""
        , "discoverChunk, essence: Essence \"pos\", actObjId: ActivePropertyId 34, statPropId: StaticPropertyId 0"
        , "discoverChunk, essence: Essence \"hp\", actObjId: ActivePropertyId 35, statPropId: StaticPropertyId 1"
        , "discover': discoverPropety returned prop"
        , "discover: discovering acting object"
        , "discoverChunk, essence: Essence \"rat\", actObjId: ActivePropertyId 31, statPropId: StaticPropertyId 11"
        , "discoverPropertiesByTypesPropertyType \"actions\""
        , "discoverPropertiesByTypesPropertyType \"inventory\""
        , "discoverChunk, essence: Essence \"pos\", actObjId: ActivePropertyId 29, statPropId: StaticPropertyId 0"
        , "discoverChunk, essence: Essence \"hp\", actObjId: ActivePropertyId 30, statPropId: StaticPropertyId 1"
        , "discover': discoverPropety returned prop"
        , "discover: discovering self not needed."
        ]

    it "Actions rotation logic test" $ do
      idCounterVar <- newTVarIO 0
      worldVar     <- newTVarIO $ World Map.empty []
      stepVar      <- newTVarIO 0
      let rndSource = mkRndSource2 stepVar
      zpNet <- atomically $ initZPNet1 idCounterVar rndSource worldVar

      atomically $ selectNextActionForObjName zpNet guard01Name
      atomically $ selectNextActionForObjName zpNet guard01Name
      atomically $ selectNextActionForObjName zpNet guard01Name
      atomically $ selectNextActionForObjName zpNet guard01Name
      atomically $ selectNextActionForObjName zpNet guard01Name
      atomically $ selectNextActionForObjName zpNet guard01Name

      verifyReport' zpNet guard01Name
        [ "Action set: Essence \"observing\""
        , "Action set: Essence \"discovering\""
        , "Action set: Essence \"setting goals\""
        , "Action set: Essence \"planning\""
        , "Action set: Essence \"following a plan\""
        , "Action set: Essence \"observing\""
        ]

    xit "Evaluating the goals setting action" $ do
      idCounterVar <- newTVarIO 0
      worldVar     <- newTVarIO $ World Map.empty []
      stepVar      <- newTVarIO 0
      let rndSource = mkRndSource2 stepVar

      zpNet <- atomically $ initZPNet1 idCounterVar rndSource worldVar

      actObj <- fromJust <$> (atomically $ getActingObject zpNet guard01Name)

      -- observing
      atomically $ selectNextAction actObj
      atomically $ evaluateCurrentAction zpNet actObj

      -- discovering
      atomically $ selectNextAction actObj
      atomically $ evaluateCurrentAction zpNet actObj

      clearReporter actObj
      clearGlobalReporter zpNet

      -- setting goals
      atomically $ selectNextAction actObj
      atomically $ evaluateCurrentAction zpNet actObj

      verifyReport actObj []

    it "Switching door states" $ do
      idCounterVar <- newTVarIO 0
      worldVar     <- newTVarIO $ World Map.empty []
      stepVar      <- newTVarIO 0
      let rndSource = mkRndSource2 stepVar

      zpNet <- atomically $ initZPNet2 idCounterVar rndSource worldVar
      actObj <- fromJust <$> (atomically $ getActingObject zpNet door01)

      outputGraph zpNet "doors.dot"

      activations <- atomically $ getAllActivations actObj

      print activations
      verifyGlobalReport zpNet []
