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

type Description = String

data PropertyValue
  = NoValue
  | PairValue PropertyValue PropertyValue
  | ListValue [PropertyValue]
  | PositionValue (Int, Int)
  | IntValue Int
  | EssenceValue Description Essence
  | ActingObjectValue ActingObject
  | ActivePropertyValue ActiveProperty
  | StaticPropertyValue StaticProperty
  | ConditionValue  -- Condition
  | TargetValue PropertyValue

data StaticPropertyValue
  = NoStaticValue
  -- Points to a target static property from the host static property.
  -- On materialization:
  --   - host static property turns into host active property
  --   - target static property turns into target active property.
  -- Host active property should have target active property as the current value.
  -- N.B. we could specify Essence instead the static proprty itself
  | MaterializableStateValue Description MaterializationLink

data MaterializationLink
  = DirectMaterialization StaticProperty
  | SharedMaterialization StaticProperty

type StaticPropertyMap = Map.Map PropertyType [MaterializationLink]

type ActivePropertyMap = Map.Map PropertyType (TVar [ActiveProperty])

data StaticPropertyDiscoverability
  = StaticDiscoverRoot      -- ^ static property can be discovered, its chidren too
  | StaticDiscoverLeaf      -- ^ static property can be discovered
  | StaticNonDiscoverable   -- ^ static property is invisible for discovery
  deriving (Show, Eq, Ord)

-- TODO: add a new type of a dynamic discoverability: discoverability on usage

data ActiveValueDiscoverability
  = ActiveValueDiscoverable      -- ^ active property value is discoverable
  | ActiveValueNonDiscoverable   -- ^ active property value is not discoverable
  deriving (Show, Eq, Ord)

data StaticProperty = StaticProperty
  { staticPropertyId       :: StaticPropertyId
  , essence                :: Essence
  , staticProperties       :: StaticPropertyMap
  , staticPropertyValueVar :: TVar StaticPropertyValue        -- Should be impure for making loops properly
  , staticPropertyDiscover :: StaticPropertyDiscoverability
  , activeValueDiscover    :: ActiveValueDiscoverability
  }

data ActiveProperty = ActiveProperty
  { activePropertyId :: ActivePropertyId
  , staticProperty   :: StaticProperty
  , propertyValueVar :: TVar PropertyValue
  , propertiesVar    :: TVar ActivePropertyMap
  }

newtype ActingObjectName = ActingObjectName String
  deriving (Show, Eq, Ord)

type KnownActingObjects = Map.Map ActingObjectId KnownActingObject

type Reporter = [String]

data ActingObject = ActingObject
  { actingObjectName      :: ActingObjectName
  , actingObjectId        :: ActingObjectId
  , rootProperty          :: ActiveProperty
  , currentActionVar      :: TVar ActiveProperty
  , actionsByEssenceVar   :: TVar (Map Essence ActiveProperty)     -- cache for quick search
  , knownActingObjectsVar :: TVar KnownActingObjects
  , actingObjectReporter  :: Maybe (TVar Reporter)
  }

data KnownActingObject = KnownActingObject
  { knownActingObjectId     :: ActingObjectId
  , rootKnownActiveProperty :: KnownActiveProperty
  }

type KnownPropertiesMap = Map.Map PropertyType [KnownActiveProperty]
data KnownActiveProperty = KnownActiveProperty
  { knownActiveProperty :: ActiveProperty
  , knownPropertiesVar  :: TVar KnownPropertiesMap
  , knownPropertyValue  :: TVar (Maybe PropertyValue)     -- Nothing means the value not known
  }

type RndSource = Int -> STM Int

type Essences = Map.Map Essence StaticProperty
data KnowledgeBase = KnowledgeBase
  { materializationLinks :: [MaterializationLink]
  , essences :: Essences
  }

type PropertiesSetter = Map.Map Essence PropertyValue

data ZPNet = ZPNet
  { knowledgeBase       :: KnowledgeBase
  , actingObjects       :: Map ActingObjectId ActingObject        -- act objs
  , actingObjectsByName :: Map ActingObjectName ActingObject      -- cache.

  , rndSource :: RndSource
  , worldVar  :: TVar World
  , zpNetReporter :: Maybe (TVar Reporter)
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
