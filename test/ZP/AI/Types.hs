{-# LANGUAGE DuplicateRecordFields #-}

module ZP.AI.Types where

import ZP.Prelude

import ZP.Types
import ZP.Game.Types

import qualified Data.Map as Map
import qualified Data.Set as Set

newtype ActivePropertyId = ActivePropertyId Int
  deriving (Show, Eq, Ord)

newtype ActingObjectId   = ActingObjectId Int
  deriving (Show, Eq, Ord)

newtype StaticPropertyId = StaticPropertyId Int
  deriving (Show, Eq, Ord)

newtype Essence = Essence String
  deriving (Show, Eq, Ord)

newtype PropertyType = PropertyType String
  deriving (Show, Eq, Ord)

data PropertyValue
  = PositionValue (Int, Int)

type ActivePropertyMap = Map.Map PropertyType (TVar [ActiveProperty])
type StaticPropertyMap = Map.Map PropertyType [StaticProperty]

data StaticPropertyDiscoverability
  = StaticDiscoverRoot      -- ^ static property can be discovered, its chidren too
  | StaticDiscoverLeaf      -- ^ static property can be discovered
  | StaticNonDiscoverable   -- ^ static property is invisible for discovery
  deriving (Show, Eq, Ord)

-- TODO: add a new type of a dynamic discoverability: discoverability on usage

data ActivePropertyDiscoverability
  = ActiveDiscoverable      -- ^ active property value is discoverable
  | ActiveNonDiscoverable   -- ^ active property value is not discoverable
  deriving (Show, Eq, Ord)

data StaticProperty = StaticProperty
  { staticPropertyId       :: StaticPropertyId
  , essence                :: Essence
  , staticProperties       :: StaticPropertyMap
  , staticPropertyDiscover :: StaticPropertyDiscoverability
  , activePropertyDiscover :: ActivePropertyDiscoverability
  }

data ActiveProperty = ActiveProperty
  { activePropertyId :: ActivePropertyId
  , staticProperty   :: StaticProperty
  , propertyValue    :: Maybe (TVar PropertyValue)
  , propertiesVar    :: TVar ActivePropertyMap
  }

newtype ActingObjectName = ActingObjectName String
  deriving (Show, Eq, Ord)

type KnownActingObjects = Map.Map ActingObjectId KnownActingObject

data ActingObject = ActingObject
  { actingObjectName      :: ActingObjectName
  , actingObjectId        :: ActingObjectId
  , rootProperty          :: ActiveProperty
  , currentActionVar      :: TVar (Maybe ActiveProperty)
  , knownActingObjectsVar :: TVar KnownActingObjects
  }

data KnownActingObject = KnownActingObject
  { knownActingObjectId     :: ActingObjectId
  , rootKnownActiveProperty :: KnownActiveProperty
  }

type KnownPropertiesMap = Map.Map PropertyType [KnownActiveProperty]
data KnownActiveProperty = KnownActiveProperty
  { knownStaticProperty :: StaticProperty
  , knownActiveProperty :: ActiveProperty
  , knownPropertiesVar  :: TVar KnownPropertiesMap
  , knownPropertyValue  :: TVar Int  --- Dummy
  }

type RndSource = Int -> STM Int

type Essences = Map.Map Essence StaticProperty
data KnowledgeBase = KnowledgeBase
  { staticProperties :: [StaticProperty]
  , essences :: Essences
  }

type PropertiesSetter = Map.Map Essence PropertyValue

data ZPNet = ZPNet
  { knowledgeBase       :: KnowledgeBase
  , actingObjects       :: Map ActingObjectId ActingObject
  , actingObjectsByName :: Map ActingObjectName ActingObject

  , rndSource :: RndSource
  , worldVar  :: TVar World
  }

data WorldObject = WorldObject
  { worldObjectId :: ActingObjectId
  , worldObjectPos :: (Int, Int)
  }

type WorldObjects = [WorldObject]

data World = World
  { level :: Level
  , worldObjects :: WorldObjects
  }

type IdCounter = TVar Int


getId :: IdCounter -> STM Int
getId idCounterVar = do
  pId <- readTVar idCounterVar
  modifyTVar' idCounterVar (+1)
  pure pId

getStaticPropertyId :: IdCounter -> STM StaticPropertyId
getStaticPropertyId idCounterVar = do
  propId <- getId idCounterVar
  pure $ StaticPropertyId propId

mkStaticProperty
  :: IdCounter
  -> TVar Essences
  -> Essence
  -> StaticPropertyMap
  -> StaticPropertyDiscoverability
  -> ActivePropertyDiscoverability
  -> STM StaticProperty
mkStaticProperty idCounterVar essencesVar essence props statDisc actDisc = do
  propId <- getStaticPropertyId idCounterVar
  let prop = StaticProperty propId essence props statDisc actDisc
  modifyTVar' essencesVar $ Map.insert essence prop
  pure prop
