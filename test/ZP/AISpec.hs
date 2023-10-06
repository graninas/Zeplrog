module ZP.AISpec where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic

import           Test.Hspec

import qualified Data.Map as Map

newtype ActivePropertyId = ActivePropertyId ObjectId
  deriving (Show, Eq, Ord)

newtype ActingObjectId   = ActingObjectId ObjectId
  deriving (Show, Eq, Ord)

newtype StaticPropertyId = StaticPropertyId ObjectId
  deriving (Show, Eq, Ord)

newtype Essence      = Essence String         -- TODO: Flyweight pattern
  deriving (Show, Eq, Ord)

newtype PropertyType = PropertyType String    -- TODO: Flyweight pattern
  deriving (Show, Eq, Ord)

type PropertyMap       = Map.Map PropertyType (TVar [ActiveProperty])
type StaticPropertyMap = Map.Map PropertyType [StaticProperty]

data ActiveProperty
  = ActiveProperty
    { propertyId :: ActivePropertyId
    , essence :: Essence
    , propertiesVar :: TVar PropertyMap
    }
  | AbstractGoalProperty
    { propertyId :: ActivePropertyId
    , goalPoint  :: StaticProperty
    }

data StaticProperty = StaticProperty StaticPropertyId Essence StaticPropertyMap

newtype ActingObjectName = ActingObjectName String
  deriving (Show, Eq, Ord)

type RndSource = Int -> STM Int

data ActingObject = ActingObject
  { name             :: ActingObjectName
  , objectId         :: ActingObjectId
  , rootProperty     :: ActiveProperty
  , currentActionVar :: TVar (Maybe ActiveProperty)
  }

data AINet = AINet
  { knowledgeBase :: [StaticProperty]
  , actingObjects :: Map ActingObjectId ActingObject

  , rndSource :: RndSource
  }

-- -----------------------------------------------------------------------------

inventoryPropType :: PropertyType
inventoryPropType = PropertyType "inventory"

actionPropType :: PropertyType
actionPropType = PropertyType "action"

abstractGoalPropType :: PropertyType
abstractGoalPropType = PropertyType "abstract goal"


observingEssence    = Essence "observing"
settingGoalsEssence = Essence "setting goals"
planningEssence     = Essence "planning"

posEssence      = Essence "pos"
hpEssence       = Essence "hp"
fireWandEssence = Essence "fire wand"
iceWandEssence  = Essence "ice wand"


-- -----------------------------------------------------------------------------

getActingObjectId :: TVar ObjectId -> STM ActingObjectId
getActingObjectId idCounterVar = do
  propId <- getObjectId idCounterVar
  pure $ ActingObjectId propId

getActivePropertyId :: TVar ObjectId -> STM ActivePropertyId
getActivePropertyId idCounterVar = do
  propId <- getObjectId idCounterVar
  pure $ ActivePropertyId propId

getStaticPropertyId :: TVar ObjectId -> STM StaticPropertyId
getStaticPropertyId idCounterVar = do
  propId <- getObjectId idCounterVar
  pure $ StaticPropertyId propId

mkProperty :: Essence -> TVar ObjectId -> STM ActiveProperty
mkProperty essence idCounterVar = do
  propId <- getActivePropertyId idCounterVar
  propsVar <- newTVar Map.empty
  pure $ ActiveProperty propId essence propsVar

mkStaticProperty :: Essence -> TVar ObjectId -> STM StaticProperty
mkStaticProperty essence idCounterVar = do
  propId <- getStaticPropertyId idCounterVar
  pure $ StaticProperty propId essence Map.empty


mkAbstractKillDogGoal :: TVar ObjectId -> StaticProperty -> STM ActiveProperty
mkAbstractKillDogGoal idCounterVar dogStatProp = do
  propId <- getActivePropertyId idCounterVar
  pure $ AbstractGoalProperty propId dogStatProp


dogStaticProperty :: TVar ObjectId -> STM StaticProperty
dogStaticProperty idCounterVar = do
  posProp <- mkStaticProperty posEssence idCounterVar
  hpProp  <- mkStaticProperty hpEssence  idCounterVar

  let propsVar = Map.fromList
        [ (inventoryPropType, [ posProp, hpProp ])
        ]

  propId <- getStaticPropertyId idCounterVar
  pure $ StaticProperty propId (Essence "dog") propsVar


guardActingObject :: TVar ObjectId -> StaticProperty -> STM (ActingObjectId, ActingObject)
guardActingObject idCounterVar dogSProp = do

  posProp       <- mkProperty posEssence      idCounterVar
  hpProp        <- mkProperty hpEssence       idCounterVar
  fireWandProp  <- mkProperty fireWandEssence idCounterVar
  iceWandProp   <- mkProperty iceWandEssence  idCounterVar

  observeAct  <- mkProperty observingEssence    idCounterVar
  setGoalsAct <- mkProperty settingGoalsEssence idCounterVar
  planAct     <- mkProperty planningEssence     idCounterVar

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

  curActVar <- newTVar Nothing

  rootPropId <- getActivePropertyId idCounterVar
  actObjId   <- getActingObjectId idCounterVar
  let rootProp = ActiveProperty rootPropId (Essence "guard") rootPropsVar
  pure (actObjId, ActingObject (ActingObjectName "guard 01") actObjId rootProp curActVar)


dogActingObject :: TVar ObjectId -> STM (ActingObjectId, ActingObject)
dogActingObject idCounterVar = do
  posProp       <- mkProperty posEssence idCounterVar
  hpProp        <- mkProperty hpEssence idCounterVar

  invVar <- newTVar [ posProp, hpProp ]

  rootPropsVar <- newTVar $ Map.fromList
    [ (inventoryPropType, invVar)
    ]

  curActVar <- newTVar Nothing

  rootPropId <- getActivePropertyId idCounterVar
  actObjId   <- getActingObjectId idCounterVar
  let rootProp = ActiveProperty rootPropId (Essence "dog") rootPropsVar
  pure (actObjId, ActingObject (ActingObjectName "dog 01") actObjId rootProp curActVar)


initialAINet :: TVar ObjectId -> RndSource -> STM (AINet, ActingObjectId)
initialAINet idCounterVar rndSource = do
  dogSProp <- dogStaticProperty idCounterVar
  let kBase = [dogSProp]

  (guardActObjId, guardActObj) <- guardActingObject idCounterVar dogSProp
  (dogActObjId, dogActObj)     <- dogActingObject idCounterVar

  let actingObjs = Map.fromList
        [ (guardActObjId, guardActObj)
        , (dogActObjId, dogActObj)
        ]
  pure (AINet kBase actingObjs rndSource, guardActObjId)

-- -----------------------------------------------------------------------------

getPropertiesOfTypeVar :: ActiveProperty -> PropertyType -> STM (Maybe (TVar [ActiveProperty]))
getPropertiesOfTypeVar (ActiveProperty _ _ propMapVar) propType = do
  propsMap <- readTVar propMapVar
  pure $ Map.lookup propType propsMap
getPropertiesOfTypeVar _ _ = pure Nothing

getPropertiesOfType :: ActiveProperty -> PropertyType -> STM [ActiveProperty]
getPropertiesOfType prop propType = do
  mbVar <- getPropertiesOfTypeVar prop propType
  case mbVar of
    Nothing  -> pure []
    Just var -> readTVar var

setCurrentAction :: ActingObject -> ActiveProperty -> STM String
setCurrentAction (ActingObject {..}) (AbstractGoalProperty {..}) = do
  writeTVar currentActionVar Nothing
  pure "Invalid property (goalProp, not activeProp)"
setCurrentAction actObj@(ActingObject {..}) actProp@(ActiveProperty {..}) = do
  writeTVar currentActionVar $ Just actProp
  pure $ "Action set: " <> show essence

selectAction' :: AINet -> ActingObject -> STM String
selectAction' aiNet@(AINet {..}) actObj@(ActingObject {..}) = do
  actProps <- getPropertiesOfType rootProperty actionPropType

  let propsCnt = length actProps

  decision <- getRandomValue rndSource propsCnt
  -- No check for validity!
  -- (decision < 0) || (decision >= (length actProps))

  case actProps of
    [] -> do
      writeTVar currentActionVar Nothing
      pure "No action properties found."
    _  -> setCurrentAction actObj $ actProps !! decision

selectAction :: AINet -> ActingObjectId -> STM String
selectAction aiNet@(AINet _ actingObjs _) objId = do
  case Map.lookup objId actingObjs of
    Nothing  -> pure $ "Acting object not found: " <> show objId
    Just obj -> selectAction' aiNet obj





-- -----------------------------------------------------------------------------

getRandomValue :: RndSource -> Int -> STM Int
getRandomValue rndSource input = rndSource input

mkRndSource1 :: TVar Int -> Int -> STM Int
mkRndSource1 stepVar input | input <= 0 = pure 0
mkRndSource1 stepVar input = do
  step <- readTVar stepVar
  modifyTVar' stepVar (+1)
  pure $ case step of
    _ -> 0


spec :: Spec
spec =
  describe "AI test" $ do
    it "Triggering the observing action" $ do
      idCounterVar <- newTVarIO $ ObjectId 0
      stepVar <- newTVarIO 0
      let rndSource = mkRndSource1 stepVar
      (aiNet, guardActObjId) <- atomically $ initialAINet idCounterVar rndSource
      result <- atomically $ selectAction aiNet guardActObjId
      result `shouldBe` "Action set: Essence \"observing\""
