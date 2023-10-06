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

import Test.Hspec

import Debug.Trace (trace)

import qualified Data.Map as Map
import qualified Data.Set as Set

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
      trace ("materializeActiveObject: Essence not found: " <> show ess) $ pure Nothing
    Just sProp -> do
      rootProp  <- materializeStaticProperty idCounterVar kb propsSetter sProp
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
dog01Name   :: ActingObjectName
dog01Name   = ActingObjectName "dog 01"
dog02Name   :: ActingObjectName
dog02Name   = ActingObjectName "dog 02"


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
  [ (noActionEssence,      PairValue (EssenceValue observingEssence)     NoValue)  -- starting point
  , (observingEssence,     PairValue (EssenceValue discoveringEssence)   NoValue)  -- no input params yet
  , (discoveringEssence,   PairValue (EssenceValue settingGoalsEssence)  NoValue)  -- no input params yet
  , (settingGoalsEssence,  PairValue (EssenceValue planningEssence)      NoValue)  -- no input params yet
  , (planningEssence,      PairValue (EssenceValue followingPlanEssence) NoValue)  -- no input params yet
  , (followingPlanEssence, PairValue (EssenceValue observingEssence)     NoValue)  -- no input params yet
  ]

initActiveObjects
  :: IdCounter
  -> (KnowledgeBase, CommonStaticProperties)
  -> STM (Map ActingObjectId ActingObject)
initActiveObjects idCounterVar (kb, commonSProps) = do
  let guardProps = Map.fromList $
        [ (posEssence, PositionValue (3, 3))
        , (hpEssence, IntValue 100)
        ] <> commonActionsLoop
  let dog01Props = Map.fromList
        [ (posEssence, PositionValue (3, 5))
        , (hpEssence, IntValue 100)
        ]
  let dog02Props = Map.fromList
        [ (posEssence, PositionValue (3, 4))
        , (hpEssence, IntValue 100)
        ]

  noActProp <- materializeStaticProperty idCounterVar kb Map.empty $ noActionSProp commonSProps

  mbObjs <- sequence
    [ materializeActiveObject idCounterVar kb noActProp guard01Name guardEssence guardProps
    , materializeActiveObject idCounterVar kb noActProp dog01Name   dogEssence   dog01Props
    , materializeActiveObject idCounterVar kb noActProp dog02Name   dogEssence   dog02Props
    ]
  pure $ Map.fromList $ map (\obj -> (actingObjectId obj, obj)) $ catMaybes mbObjs


initZPNet
  :: IdCounter
  -> RndSource
  -> TVar World
  -> STM ZPNet
initZPNet idCounterVar rndSource worldVar = do
  kb'@(kb, _) <- initKnowledgeBase idCounterVar
  actObjs     <- initActiveObjects idCounterVar kb'
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


verifyReport :: ActingObject -> Report -> IO ()
verifyReport ActingObject {actingObjectReporter} expectedReport =
  case actingObjectReporter of
    Nothing -> pure ()
    Just reporterVar -> do
      report <- readTVarIO reporterVar
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
      report <- readTVarIO reporterVar
      reverse report `shouldBe` expectedReport


spec :: Spec
spec =
  describe "AI test" $ do
    it "Selecting the next action. Should be observing (after noAct)" $ do
      idCounterVar <- newTVarIO 0
      worldVar     <- newTVarIO $ World Map.empty []
      stepVar      <- newTVarIO 0
      let rndSource = mkRndSource1 stepVar

      zpNet  <- atomically $ initZPNet idCounterVar rndSource worldVar
      atomically $ selectNextActionForObjName zpNet guard01Name

      verifyReport' zpNet guard01Name ["Action set: Essence \"observing\""]

    it "Evaluating the observing action" $ do
      idCounterVar <- newTVarIO 0
      worldVar     <- newTVarIO $ World Map.empty []
      stepVar      <- newTVarIO 0
      let rndSource = mkRndSource2 stepVar

      zpNet   <- atomically $ initZPNet idCounterVar rndSource worldVar
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

      zpNet <- atomically $ initZPNet idCounterVar rndSource worldVar

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
        , "discoverChunk, essence: Essence \"dog\", actObjId: ActivePropertyId 37, statPropId: StaticPropertyId 11"
        , "discoverPropertiesByTypesPropertyType \"actions\""
        , "discoverPropertiesByTypesPropertyType \"inventory\""
        , "discoverChunk, essence: Essence \"pos\", actObjId: ActivePropertyId 39, statPropId: StaticPropertyId 0"
        , "discoverChunk, essence: Essence \"hp\", actObjId: ActivePropertyId 40, statPropId: StaticPropertyId 1"
        , "discover': discoverPropety returned prop"
        , "discover: discovering acting object"
        , "discoverChunk, essence: Essence \"dog\", actObjId: ActivePropertyId 32, statPropId: StaticPropertyId 11"
        , "discoverPropertiesByTypesPropertyType \"actions\""
        , "discoverPropertiesByTypesPropertyType \"inventory\""
        , "discoverChunk, essence: Essence \"pos\", actObjId: ActivePropertyId 34, statPropId: StaticPropertyId 0"
        , "discoverChunk, essence: Essence \"hp\", actObjId: ActivePropertyId 35, statPropId: StaticPropertyId 1"
        , "discover': discoverPropety returned prop"
        , "discover: discovering self not needed."
        ]


    it "Actions rotation logic test" $ do
      idCounterVar <- newTVarIO 0
      worldVar     <- newTVarIO $ World Map.empty []
      stepVar      <- newTVarIO 0
      let rndSource = mkRndSource2 stepVar
      zpNet <- atomically $ initZPNet idCounterVar rndSource worldVar

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
