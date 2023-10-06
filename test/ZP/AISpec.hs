module ZP.AISpec where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic

import           Test.Hspec

import qualified Data.Map as Map

newtype ActivePropertyId = ActivePropertyId ObjectId
newtype StaticPropertyId = StaticPropertyId ObjectId

newtype Essence      = Essence String         -- TODO: Flyweight pattern
newtype PropertyType = PropertyType String    -- TODO: Flyweight pattern
  deriving (Show, Eq, Ord)

type PropertyMap = Map.Map PropertyType (TVar [TVar ActiveProperty])
type StaticPropertyMap = Map.Map PropertyType [StaticProperty]

data ActiveProperty
  = ActiveProperty ActivePropertyId Essence (TVar PropertyMap)
  | AbstractGoalProperty ActivePropertyId StaticProperty

data StaticProperty = StaticProperty StaticPropertyId Essence StaticPropertyMap


newtype Name = Name String

data ActingObject = ActingObject
  { name :: Name
  , rootProperty :: ActiveProperty
  }

data AINet = AINet
  { knowledgeBase :: [StaticProperty]
  , actingObjects :: [ActingObject]
  }

inventoryPropType :: PropertyType
inventoryPropType = PropertyType "inventory"

actionPropType :: PropertyType
actionPropType = PropertyType "action"

abstractGoalPropType :: PropertyType
abstractGoalPropType = PropertyType "abstract goal"

getActivePropertyId :: TVar ObjectId -> STM ActivePropertyId
getActivePropertyId idCounterVar = do
  propId <- getObjectId idCounterVar
  pure $ ActivePropertyId propId

getStaticPropertyId :: TVar ObjectId -> STM StaticPropertyId
getStaticPropertyId idCounterVar = do
  propId <- getObjectId idCounterVar
  pure $ StaticPropertyId propId

mkProperty :: String -> TVar ObjectId -> STM (TVar ActiveProperty)
mkProperty essenceStr idCounterVar = do
  propId <- getActivePropertyId idCounterVar
  propsVar <- newTVar Map.empty
  newTVar $ ActiveProperty propId (Essence essenceStr) propsVar

mkStaticProperty :: String -> TVar ObjectId -> STM StaticProperty
mkStaticProperty essenceStr idCounterVar = do
  propId <- getStaticPropertyId idCounterVar
  pure $ StaticProperty propId (Essence essenceStr) Map.empty


mkAbstractKillDogGoal :: TVar ObjectId -> StaticProperty -> STM (TVar ActiveProperty)
mkAbstractKillDogGoal idCounterVar dogStatProp = do
  propId <- getActivePropertyId idCounterVar
  newTVar $ AbstractGoalProperty propId dogStatProp


dogStaticProperty :: TVar ObjectId -> STM StaticProperty
dogStaticProperty idCounterVar = do
  posProp <- mkStaticProperty "pos" idCounterVar
  hpProp  <- mkStaticProperty "hp"  idCounterVar

  let propsVar = Map.fromList
        [ (inventoryPropType, [ posProp, hpProp ])
        ]

  propId <- getStaticPropertyId idCounterVar
  pure $ StaticProperty propId (Essence "dog") propsVar


guardActingObject :: TVar ObjectId -> StaticProperty -> STM ActingObject
guardActingObject idCounterVar dogSProp = do

  posProp       <- mkProperty "pos" idCounterVar
  hpProp        <- mkProperty "hp" idCounterVar
  fireWandProp  <- mkProperty "fire wand" idCounterVar
  iceWandProp   <- mkProperty "ice wand" idCounterVar

  observeAct  <- mkProperty "observing"     idCounterVar
  setGoalsAct <- mkProperty "setting goals" idCounterVar
  planAct     <- mkProperty "planning"      idCounterVar

  killDogGoal <- mkAbstractKillDogGoal idCounterVar dogSProp

  invVar <- newTVar
      [ posProp
      , hpProp
      , fireWandProp
      , iceWandProp
      ]

  actsVar <- newTVar
    [ observeAct
    , setGoalsAct
    , planAct
    ]

  goalsVar <- newTVar [killDogGoal]

  rootPropsVar <- newTVar $ Map.fromList
    [ (inventoryPropType, invVar)
    , (actionPropType, actsVar)
    , (abstractGoalPropType, goalsVar)
    ]

  rootPropId <- getActivePropertyId idCounterVar
  let rootProp = ActiveProperty rootPropId (Essence "guard") rootPropsVar
  pure $ ActingObject (Name "guard 01") rootProp



dogActingObject :: TVar ObjectId -> STM ActingObject
dogActingObject idCounterVar = do
  posProp       <- mkProperty "pos" idCounterVar
  hpProp        <- mkProperty "hp" idCounterVar

  invVar <- newTVar [ posProp, hpProp ]

  rootPropsVar <- newTVar $ Map.fromList
    [ (inventoryPropType, invVar)
    ]

  rootPropId <- getActivePropertyId idCounterVar
  let rootProp = ActiveProperty rootPropId (Essence "dog") rootPropsVar
  pure $ ActingObject (Name "dog 01") rootProp


initialAINet :: TVar ObjectId -> STM AINet
initialAINet idCounterVar = do

  dogSProp <- dogStaticProperty idCounterVar

  let kBase = [dogSProp]

  actingObjs <- sequence
    [ guardActingObject idCounterVar dogSProp
    , dogActingObject idCounterVar
    ]

  pure $ AINet kBase actingObjs


spec :: Spec
spec =
  describe "AI test" $ do
    it "Simple object discovery" $ do
      idCounterVar <- newTVarIO $ ObjectId 0
      AINet kBase actingObjs <- atomically $ initialAINet idCounterVar

      length actingObjs `shouldBe` 2
