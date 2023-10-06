{-# LANGUAGE DataKinds #-}

module ZP.Domain.Static.Model where

import Prelude (Bool)
import GHC.TypeLits

------ Common and General -----------------

-- | Sudo ID of a property
data Essence where
  Ess :: Symbol -> Essence

-- | Value definition

data ValDefType where
  IntValDef      :: Nat -> ValDefType
  IntRangeValDef :: (Nat, Nat) -> ValDefType
  BoolValDef     :: Bool -> ValDefType
  PairValDef     :: ValDefType -> ValDefType -> ValDefType

-- | Variable definition

type VarName = Symbol

data VarDef where
  IntVar        :: VarName -> VarDef
  IntRangeVar   :: VarName -> VarDef
  BoolVar       :: VarName -> VarDef
  PairVar       :: VarName -> VarDef -> VarDef -> VarDef

------ Query and Procedure Script ------------


data QuerySetting where
  FollowReferences :: QuerySetting

data CompareOp where
  Eq :: CompareOp

data QueryTerm where
  QEssence :: Essence -> QueryTerm
  QGetEssence :: QueryTerm

type QueryPath = [QueryTerm]

data Query where
  SimpleQuery
    :: [QuerySetting]
    -> QueryPath
    -> VarDef
    -> Query

data Condition where
  ConditionDef
    :: VarName
    -> CompareOp
    -> ValDefType
    -> Condition

data Procedure where
  ReplaceProp
    :: [Essence]
    -> Property
    -> Procedure

data Action where
  ConditionalAction
    :: Condition
    -> Procedure
    -> Action

data Script where
  SimpleScript
    :: Essence
    -> [Query]       -- ^ Query specific values before the script
    -> [Action]
    -> Script


------ Property -----


-- | Used to make property hierarchies.
-- Essence arg: own essence
-- Property arg: parent property
data PropertyRoot where
  EssRoot   :: Essence -> PropertyRoot
  PropRoot  :: Essence -> Property -> PropertyRoot

-- | Used to make static property hierarchies.
-- Essence arg: own essence
-- Property arg: parent property
data StaticPropertyRoot where
  EssStaticRoot   :: Essence -> StaticPropertyRoot
  PropStaticRoot  :: Essence -> StaticProperty -> StaticPropertyRoot

-- Property owning replaces materialization from the prev version
data PropertyOwning where
  -- | Property will be materialized for each parent prop.
  OwnProp    :: Property -> PropertyOwning
  -- | Property will be materialized only once and shared between parents.
  SharedProp :: Property -> PropertyOwning


type Category = Essence

data PropertyKeyValue where
  -- | Implicit dictionary of properties.
  -- When materialized, becomes a dict with keys taken from properties
  PropKeyBag :: Category -> [PropertyOwning] -> PropertyKeyValue
  -- | Separate property
  PropKeyVal :: Category -> PropertyOwning -> PropertyKeyValue

data StaticProperty where
  StaticProp :: StaticPropertyRoot -> StaticProperty

data Property where
  -- | Static prop reference. Will not be materialized.
  StaticPropRef
    :: StaticProperty
    -> Property
  -- | Leaf prop. Points to another prop with an essence path.
  --   Will be materialized as a const.
  PropRef
    :: [Essence]
    -> Property
  -- | Lear prop. Value will be materialized as const.
  PropConst
    :: PropertyRoot
    -> ValDefType
    -> Property
  -- | Lear prop. Value will be materialized as mutable var (TVar).
  PropVal
    :: PropertyRoot
    -> ValDefType
    -> Property
  -- | Compound property.
  -- Each prop in the bag is a mutable reference.
  -- Each prop can be replaced by some other prop.
  PropDict
    :: PropertyRoot
    -> [PropertyKeyValue]
    -> Property

  PropScript
    :: Script
    -> Property

