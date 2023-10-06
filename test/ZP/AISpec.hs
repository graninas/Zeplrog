module ZP.AISpec where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic

import           Test.Hspec

import qualified Data.Map as Map

type PropertyId = ObjectId

newtype Essence = Essence String
data Property = Property PropertyId Essence [TVar Property]

newtype Name = Name String

data ActingObject = ActingObject Name (TVar Property)

discoveryEssense :: Essence
discoveryEssense = Essence "discovery"

playerEssense :: Essence
playerEssense = Essence "0"

dogEssense :: Essence
dogEssense = Essence "d"

guardEssense :: Essence
guardEssense = Essence "@"

mkProperty :: String -> TVar ObjectId -> STM (TVar Property)
mkProperty essenceStr idCounterVar = do
  propId <- getObjectId idCounterVar
  newTVar $ Property propId (Essence essenceStr) []

playerDiscoveryProperty ::  TVar ObjectId -> STM (TVar Property)
playerDiscoveryProperty idCounterVar = do
  propId <- getObjectId idCounterVar
  newTVar $ Property propId discoveryEssense []

playerCharacterProperty :: TVar ObjectId -> STM (TVar Property)
playerCharacterProperty idCounterVar = do
  discoveryProp <- playerDiscoveryProperty idCounterVar
  hpProp <- mkProperty "HP" idCounterVar
  propId <- getObjectId idCounterVar
  newTVar $ Property propId playerEssense [discoveryProp, hpProp]

guardProperty :: TVar ObjectId -> STM (TVar Property)
guardProperty idCounterVar = do
  hpProp        <- mkProperty "HP" idCounterVar
  iceWandProp   <- mkProperty "ice wand" idCounterVar
  fireWandProp  <- mkProperty "fire wand" idCounterVar
  -- attackDogStaticGoal
  hpProp <- mkProperty "HP" idCounterVar
  propId <- getObjectId idCounterVar
  newTVar $ Property propId guardEssense [hpProp, iceWandProp, fireWandProp]

dogProperty :: TVar ObjectId -> STM (TVar Property)
dogProperty idCounterVar = do
  hpProp <- mkProperty "HP" idCounterVar
  propId <- getObjectId idCounterVar
  newTVar $ Property propId dogEssense [hpProp]



initialObjects :: TVar ObjectId -> STM [ActingObject]
initialObjects idCounterVar = do
  plPropVar    <- playerCharacterProperty idCounterVar
  guardPropVar <- guardProperty idCounterVar
  dogPropVar <- dogProperty idCounterVar
  pure [ ActingObject (Name "player") plPropVar
       , ActingObject (Name "guard") guardPropVar
       , ActingObject (Name "dog") dogPropVar
       ]




spec :: Spec
spec =
  describe "AI test" $ do
    it "Simple object discovery" $ do
      idCounterVar <- newTVarIO $ ObjectId 0
      objs <- atomically $ initialObjects idCounterVar

      length objs `shouldBe` 3
