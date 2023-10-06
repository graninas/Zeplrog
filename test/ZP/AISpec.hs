{-# LANGUAGE DuplicateRecordFields #-}

module ZP.AISpec where

import ZP.Prelude
import ZP.Types
import ZP.App
import ZP.Game.Types
import ZP.Game.Logic

import           Test.Hspec

import Debug.Trace (trace)

import qualified Data.Map as Map
import qualified Data.Set as Set

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

data StaticPropertyDiscoverability
  = StaticDiscoverRoot
  | StaticDiscoverLeaf
  | StaticNonDiscoverable
  deriving (Show, Eq, Ord)

data ActivePropertyDiscoverability
  = ActiveDiscoverable
  | ActiveNonDiscoverable
  deriving (Show, Eq, Ord)

data StaticProperty = StaticProperty
  { propertyId :: StaticPropertyId
  , essence    :: Essence
  , properties :: StaticPropertyMap
  , staticPropertyDiscover :: StaticPropertyDiscoverability
  , activePropertyDiscover :: ActivePropertyDiscoverability
  }

data ActiveProperty = ActiveProperty
  { propertyId    :: ActivePropertyId
  , essence       :: Essence
  , staticPoint   :: Maybe StaticProperty
  , propertiesVar :: TVar PropertyMap
  }

newtype ActingObjectName = ActingObjectName String
  deriving (Show, Eq, Ord)

type KnownActingObjects = Map.Map ActingObjectId KnownActingObject

data ActingObject = ActingObject
  { name             :: ActingObjectName
  , objectId         :: ActingObjectId
  , rootProperty     :: ActiveProperty
  , currentActionVar :: TVar (Maybe ActiveProperty)
  , knownActingObjectsVar :: TVar KnownActingObjects
  }

data KnownActingObject = KnownActingObject
  { knownActingObjectId  :: ActingObjectId
  , rootKnownActiveProperty :: KnownActiveProperty
  }

type KnownPropertiesMap = Map.Map PropertyType [KnownActiveProperty]
data KnownActiveProperty = KnownActiveProperty
  { staticPoint        :: StaticProperty
  , activeProperty     :: ActiveProperty
  , knownPropertiesVar :: TVar KnownPropertiesMap
  , knownPropertyValue :: TVar Int  --- Dummy
  }

type RndSource = Int -> STM Int

data AINet = AINet
  { knowledgeBase :: [StaticProperty]
  , actingObjects :: Map ActingObjectId ActingObject

  , rndSource :: RndSource
  , worldVar  :: TVar World
  }

data WorldObject = WorldObject
  { actingObjectId :: ActingObjectId
  , worldObjectPos :: (Int, Int)
  }

type WorldObjects = [WorldObject]

data World = World
  { level :: Level
  , worldObjects :: WorldObjects
  }


-- data WorldEntry = WE
--   { pos :: (Int, Int)
--   , objIdx :: Int
--   , staticPropIdx :: Int
--   }
--   deriving (Show, Read, Eq, Ord)
--
-- type WorldEntries = [WorldEntry]

-- -----------------------------------------------------------------------------
--
-- loadWorld :: String -> IO World
-- loadWorld lvlFileName = do
--   l1 :: [String] <- (reverse . map T.unpack . lines) <$> (readFile lvlFileName)
--   let l2 :: [(Int, String)] = zip [1..] l1
--   let l3 :: [ (CellIdxs, Char) ] = join $ map zipRow l2
--   pure $ Map.fromList l3
--   where
--     zipRow :: (Int, String) -> [ (CellIdxs, Char) ]
--     zipRow (y, str) = [ (CellIdxs (x, y), ch) | (x, ch) <- zip [1..] str ]
--

loadTestWorld :: String -> WorldObjects -> IO World
loadTestWorld lvlFileName objs = do
  lvl <- loadLevel lvlFileName
  pure $ World lvl objs

-- -----------------------------------------------------------------------------

inventoryPropType :: PropertyType
inventoryPropType = PropertyType "inventory"

actionPropType :: PropertyType
actionPropType = PropertyType "action"

abstractGoalPropType :: PropertyType
abstractGoalPropType = PropertyType "abstract goal"

knownActingObjectsPropType :: PropertyType
knownActingObjectsPropType = PropertyType "known acting objects"

abstractGoalEssence = Essence "abstract goal"
observingEssence    = Essence "observing"
settingGoalsEssence = Essence "setting goals"
planningEssence     = Essence "planning"

posEssence      = Essence "pos"
hpEssence       = Essence "hp"
fireWandEssence = Essence "fire wand"
iceWandEssence  = Essence "ice wand"
dogEssence      = Essence "dog"
guardEssence    = Essence "guard"

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
  pure $ ActiveProperty propId essence Nothing propsVar

mkStaticProperty
  :: Essence
  -> TVar ObjectId
  -> StaticPropertyDiscoverability
  -> ActivePropertyDiscoverability
  -> STM StaticProperty
mkStaticProperty essence idCounterVar statDisc actDisc = do
  propId <- getStaticPropertyId idCounterVar
  pure $ StaticProperty propId essence Map.empty statDisc actDisc


mkAbstractKillDogGoal :: TVar ObjectId -> StaticProperty -> STM ActiveProperty
mkAbstractKillDogGoal idCounterVar dogStatProp = do
  propId <- getActivePropertyId idCounterVar
  propsVar <- newTVar Map.empty
  pure $ ActiveProperty propId abstractGoalEssence (Just dogStatProp) propsVar


dogStaticProperty :: TVar ObjectId -> STM StaticProperty
dogStaticProperty idCounterVar = do
  posProp <- mkStaticProperty posEssence idCounterVar StaticDiscoverLeaf ActiveDiscoverable
  hpProp  <- mkStaticProperty hpEssence  idCounterVar StaticDiscoverLeaf ActiveNonDiscoverable

  let propsVar = Map.fromList
        [ (inventoryPropType, [ posProp, hpProp ])
        ]

  propId <- getStaticPropertyId idCounterVar
  pure $ StaticProperty propId dogEssence propsVar StaticDiscoverRoot ActiveNonDiscoverable


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
  let rootProp = ActiveProperty rootPropId guardEssence Nothing rootPropsVar
  knownObjsVar <- newTVar Map.empty
  let actObj = ActingObject (ActingObjectName "guard 01") actObjId rootProp curActVar knownObjsVar
  pure
    -- $ traceShow actObjId
    $ (actObjId, actObj)


dogActingObject :: TVar ObjectId -> StaticProperty -> STM (ActingObjectId, ActingObject)
dogActingObject idCounterVar dosSProp = do
  posProp <- mkProperty posEssence idCounterVar
  hpProp  <- mkProperty hpEssence idCounterVar

  invVar <- newTVar [ posProp, hpProp ]

  rootPropsVar <- newTVar $ Map.fromList
    [ (inventoryPropType, invVar)
    ]

  curActVar <- newTVar Nothing

  rootPropId <- getActivePropertyId idCounterVar
  actObjId   <- getActingObjectId idCounterVar
  let rootProp = ActiveProperty rootPropId dogEssence (Just dosSProp) rootPropsVar

  knownObjsVar <- newTVar Map.empty
  let actObj = ActingObject (ActingObjectName "dog 01") actObjId rootProp curActVar knownObjsVar
  pure
    -- $ traceShow actObjId
    $ (actObjId, actObj)


initialAINet :: TVar ObjectId -> RndSource -> TVar World -> STM (AINet, ActingObjectId)
initialAINet idCounterVar rndSource worldVar = do
  dogSProp <- dogStaticProperty idCounterVar
  let kBase = [dogSProp]

  (guardActObjId, guardActObj) <- guardActingObject idCounterVar dogSProp
  (dogActObjId, dogActObj)     <- dogActingObject idCounterVar dogSProp

  let actingObjs = Map.fromList
        [ (guardActObjId, guardActObj)
        , (dogActObjId, dogActObj)
        ]
  pure (AINet kBase actingObjs rndSource worldVar, guardActObjId)

-- -----------------------------------------------------------------------------

getPropertiesOfTypeVar :: ActiveProperty -> PropertyType -> STM (Maybe (TVar [ActiveProperty]))
getPropertiesOfTypeVar (ActiveProperty {..}) propType = do
  props <- readTVar propertiesVar
  pure $ Map.lookup propType props

getPropertiesOfType :: ActiveProperty -> PropertyType -> STM [ActiveProperty]
getPropertiesOfType prop propType = do
  mbVar <- getPropertiesOfTypeVar prop propType
  case mbVar of
    Nothing  -> pure []
    Just var -> readTVar var

setCurrentAction :: ActingObject -> ActiveProperty -> STM String
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
selectAction aiNet@(AINet {..}) objId = do
  case Map.lookup objId actingObjects of
    Nothing  -> pure $ "Acting object not found: " <> show objId
    Just obj -> selectAction' aiNet obj

--------

observe :: AINet -> ActingObject -> STM [ActingObject]
observe aiNet@(AINet {actingObjects, worldVar}) _ = do
  -- Initial observing logic.
  -- Should be improved to use local world properties to do observing.
  World {worldObjects} <- readTVar worldVar
  let objIds = map actingObjectId worldObjects
  pure $ Map.elems actingObjects

isAlreadyKnownActingObject :: ActingObject -> ActingObject -> STM Bool
isAlreadyKnownActingObject self ActingObject{objectId} = do
  knownObjs <- readTVar $ knownActingObjectsVar self
  pure $ Map.member objectId knownObjs


discoverPropertiesByTypes
  :: (PropertyType, TVar [ActiveProperty])
  -> STM (PropertyType, [KnownActiveProperty])
discoverPropertiesByTypes (propType, activePropsVar) = do
  trace ("discoverPropertiesByTypes" <> show propType) $ pure ()
  activeProps <- readTVar activePropsVar
  mbProps     <- mapM discoverProperty activeProps
  pure (propType, catMaybes mbProps)


discoverChunk :: StaticProperty -> ActiveProperty -> STM KnownActiveProperty
discoverChunk statProp activeProp@(ActiveProperty {propertiesVar}) = do
  trace "discoverChunk" $ pure ()
  activeProps   <- readTVar propertiesVar
  knownProps    <- mapM discoverPropertiesByTypes $ Map.toList activeProps
  knownPropsVar <- newTVar $ Map.fromList knownProps
  knownVal      <- newTVar 0    -- known prop val dummy
  pure $ KnownActiveProperty statProp activeProp knownPropsVar knownVal


discoverProperty :: ActiveProperty -> STM (Maybe KnownActiveProperty)
discoverProperty activeProp@(ActiveProperty{staticPoint}) = do
  trace "discoverProperty" $ pure ()
  case staticPoint of
    Nothing -> trace "discoverProperty: prop doesn't have static prop to discover." $ pure Nothing
    Just statProp@(StaticProperty {..})
      | staticPropertyDiscover == StaticDiscoverRoot -> Just <$> discoverChunk statProp activeProp
      | staticPropertyDiscover == StaticDiscoverLeaf -> Just <$> discoverChunk statProp activeProp
      | staticPropertyDiscover == StaticNonDiscoverable ->
          trace "discoverProperty: static prop is not discoverable." $ pure Nothing

discover' :: ActingObject -> ActingObject -> STM ()
discover' ActingObject{knownActingObjectsVar} other@(ActingObject{objectId}) = do
  mbKnownActiveProp <- discoverProperty $ rootProperty other
  case mbKnownActiveProp of
    Nothing -> trace "discover': discoveryPropety returned Nothing" $ pure ()
    Just knownActiveProp -> do
      let knownObj = KnownActingObject objectId knownActiveProp
      knownObjs <- readTVar knownActingObjectsVar
      writeTVar knownActingObjectsVar $ Map.insert objectId knownObj knownObjs
      trace "discover': discoverPropety returned prop" $ pure ()

discover :: AINet -> ActingObject -> ActingObject -> STM ()
discover _ self other | objectId self == objectId other = do
  trace "discover: discovering self not needed." $ pure ()
discover aiNet self other = do
  known <- isAlreadyKnownActingObject self other
  when (not known) $ do
    trace "discover: discovering acting object" $ pure ()
    discover' self other
  when known $ trace "discover: known object" $ pure ()


evaluateObservingAction :: AINet -> ActingObject -> STM String
evaluateObservingAction aiNet@(AINet {..}) actObj@(ActingObject {..}) = do
  objs <- observe aiNet actObj
  mapM_ (discover aiNet actObj) objs
  pure "Observing action"

evaluateGoalSettingAction :: AINet -> ActingObject -> STM String
evaluateGoalSettingAction _ _ = pure "Goal setting action"

evaluatePlanningAction :: AINet -> ActingObject -> STM String
evaluatePlanningAction _ _ = pure "Planning action"

evaluateCurrentAction' :: AINet -> ActingObject -> STM String
evaluateCurrentAction' aiNet@(AINet {..}) actObj@(ActingObject {..}) = do
  mbActiveProperty <- readTVar currentActionVar
  case mbActiveProperty of
    Nothing -> pure "No action."
    Just (ActiveProperty {..}) -> case () of
      () | essence == observingEssence    -> evaluateObservingAction aiNet actObj
      () | essence == settingGoalsEssence -> evaluateGoalSettingAction aiNet actObj
      () | essence == planningEssence     -> evaluatePlanningAction aiNet actObj
      _ -> pure $ "Action is not yet supported: " <> show essence

evaluateCurrentAction :: AINet -> ActingObjectId -> STM String
evaluateCurrentAction aiNet@(AINet {..}) objId = do
  case Map.lookup objId actingObjects of
    Nothing  -> pure $ "Acting object not found: " <> show objId
    Just obj -> evaluateCurrentAction' aiNet obj


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
      idCounterVar <- newTVarIO $ ObjectId 0
      worldVar <- newTVarIO $ World Map.empty []
      stepVar <- newTVarIO 0
      let rndSource = mkRndSource1 stepVar

      (aiNet, guardActObjId) <- atomically $ initialAINet idCounterVar rndSource worldVar
      result <- atomically $ selectAction aiNet guardActObjId
      result `shouldBe` "Action set: Essence \"observing\""

    it "Evaluating the observing action" $ do
      idCounterVar <- newTVarIO $ ObjectId 0
      worldVar <- newTVarIO $ World Map.empty []
      stepVar <- newTVarIO 0
      let rndSource = mkRndSource2 stepVar
      (aiNet, guardActObjId) <- atomically $ initialAINet idCounterVar rndSource worldVar

      let testObjects =
            [ WorldObject (ActingObjectId $ ObjectId 12) (2,2)
            , WorldObject (ActingObjectId $ ObjectId 16) (6,2)
            ]
      world <- loadTestWorld "./test/test_data/lvl1.zpg" testObjects
      atomically $ writeTVar worldVar world


      result1 <- atomically $ selectAction aiNet guardActObjId
      result2 <- atomically $ evaluateCurrentAction aiNet guardActObjId

      result1 `shouldBe` "Action set: Essence \"observing\""
      result2 `shouldBe` "Observing action"
