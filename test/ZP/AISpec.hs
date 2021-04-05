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


materializeActiveObject
  :: IdCounter
  -> KnowledgeBase
  -> ActiveProperty
  -> ActingObjectName
  -> Essence
  -> PropertiesSetter
  -> STM (Maybe ActingObject)
materializeActiveObject idCounterVar kb@(KnowledgeBase {essences}) noActProp name essence propsSetter = do
  case Map.lookup essence essences of
    Nothing ->
      trace ("materializeActiveObject: Essence not found: " <> show essence) $ pure Nothing
    Just sProp -> do
      rootProp     <- materializeStaticProperty idCounterVar kb propsSetter sProp
      knownObjsVar <- newTVar Map.empty
      curActVar    <- newTVar noActProp
      actObjId     <- getActingObjectId idCounterVar
      pure $ Just $ ActingObject name actObjId rootProp curActVar knownObjsVar


guard01Name :: ActingObjectName
guard01Name = ActingObjectName "guard 01"
dog01Name   :: ActingObjectName
dog01Name   = ActingObjectName "dog 01"
dog02Name   :: ActingObjectName
dog02Name   = ActingObjectName "dog 02"

commonActionsLoop :: [(Essence, PropertyValue)]
commonActionsLoop =
  [ (observingEssence,     PairValue (EssenceValue discoveringEssence)   NoValue)  -- no input params yet
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
  let actObjsByName = Map.fromList $ map (\(_, o) -> (actingObjectName o, o)) $ Map.toList actObjs
  pure $ ZPNet kb actObjs actObjsByName rndSource worldVar


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


spec :: Spec
spec =
  describe "AI test" $ do
    it "Selecting the observing action" $ do
      idCounterVar <- newTVarIO 0
      worldVar     <- newTVarIO $ World Map.empty []
      stepVar      <- newTVarIO 0
      let rndSource = mkRndSource1 stepVar

      aiNet  <- atomically $ initZPNet idCounterVar rndSource worldVar
      result <- atomically $ selectAction aiNet guard01Name
      result `shouldBe` "Action set: Essence \"observing\""

    it "Evaluating the observing & discovering actions" $ do
      idCounterVar <- newTVarIO 0
      worldVar     <- newTVarIO $ World Map.empty []
      stepVar      <- newTVarIO 0
      let rndSource = mkRndSource2 stepVar
      aiNet <- atomically $ initZPNet idCounterVar rndSource worldVar

      result1 <- atomically $ selectAction aiNet guard01Name
      result2 <- atomically $ evaluateCurrentAction aiNet guard01Name

      result1 `shouldBe` "Action set: Essence \"observing\""
      result2 `shouldBe` "Observing action"

    it "Actions rotation logic test" $ do
      idCounterVar <- newTVarIO 0
      worldVar     <- newTVarIO $ World Map.empty []
      stepVar      <- newTVarIO 0
      let rndSource = mkRndSource2 stepVar
      aiNet <- atomically $ initZPNet idCounterVar rndSource worldVar

      result1 <- atomically $ selectNextAction aiNet guard01Name
      result2 <- atomically $ evaluateCurrentAction aiNet guard01Name

      result1 `shouldBe` "Action set: Essence \"observing\""
      result2 `shouldBe` "Observing action"
