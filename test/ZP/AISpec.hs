module ZP.AISpec where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic

import           Test.Hspec

import qualified Data.Map as Map

type ActivePropertyId = ObjectId

newtype Essence      = Essence String         -- TODO: Flyweight pattern
newtype PropertyType = PropertyType String    -- TODO: Flyweight pattern
  deriving (Show, Eq, Ord)

type PropertyMap = Map.Map PropertyType (TVar [TVar ActiveProperty])

data ActiveProperty
  = ActiveProperty ActivePropertyId Essence (TVar PropertyMap)

newtype Name = Name String


data ActingObject = ActingObject
  { name :: Name
  , rootProperty :: ActiveProperty
  }

inventoryPropType :: PropertyType
inventoryPropType = PropertyType "inventory"

actionPropType :: PropertyType
actionPropType = PropertyType "action"

abstractGoalPropType :: PropertyType
abstractGoalPropType = PropertyType "abstract goal"

abstractPointPropType :: PropertyType
abstractPointPropType = PropertyType "abstract point"


mkProperty :: String -> TVar ObjectId -> STM (TVar ActiveProperty)
mkProperty essenceStr idCounterVar = do
  propId <- getObjectId idCounterVar
  propsVar <- newTVar Map.empty
  newTVar $ ActiveProperty propId (Essence essenceStr) propsVar


mkAbstractKillDogGoal :: TVar ObjectId -> STM (TVar ActiveProperty)
mkAbstractKillDogGoal idCounterVar = do

  let abstractDogProp = undefined    -- TODO

  propsVar <- newTVar $ Map.fromList
    [ (abstractPointPropType, abstractDogProp)
    ]

  propId <- getObjectId idCounterVar
  newTVar $ ActiveProperty propId (Essence "kill") propsVar




guardActingObject :: TVar ObjectId -> STM ActingObject
guardActingObject idCounterVar = do
  posProp       <- mkProperty "pos" idCounterVar
  hpProp        <- mkProperty "hp" idCounterVar
  fireWandProp  <- mkProperty "fire wand" idCounterVar
  iceWandProp   <- mkProperty "ice wand" idCounterVar

  observeAct  <- mkProperty "observing"     idCounterVar
  setGoalsAct <- mkProperty "setting goals" idCounterVar
  planAct     <- mkProperty "planning"      idCounterVar

  killDogGoal <- mkAbstractKillDogGoal idCounterVar

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

  rootPropId <- getObjectId idCounterVar
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

  rootPropId <- getObjectId idCounterVar
  let rootProp = ActiveProperty rootPropId (Essence "dog") rootPropsVar
  pure $ ActingObject (Name "dog 01") rootProp


initialObjects :: TVar ObjectId -> STM [ActingObject]
initialObjects idCounterVar =
  sequence
    [ guardActingObject idCounterVar
    , dogActingObject idCounterVar
    ]


spec :: Spec
spec =
  describe "AI test" $ do
    it "Simple object discovery" $ do
      idCounterVar <- newTVarIO $ ObjectId 0
      objs <- atomically $ initialObjects idCounterVar

      length objs `shouldBe` 3
