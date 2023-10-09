{-# LANGUAGE DataKinds #-}

module ZP.Domain.Static.Model where

import ZP.Prelude
import GHC.TypeLits

------ Common and General -----------------

-- | Specific mechanism (a kind of the HKD pattern)
-- to choose between Symbol and String, Nat and Int
-- when using the model either as type-level or as value-level one.
-- Very boilerplaity. Has a high accidental complexity.
-- Alternative: a completely separate yet almost identical
-- of set of types for the static value-level model
-- with Strings instead Symbols and Ints instead Nats.

data Level = TypeLevel | ValueLevel

type family StringType (lvl :: Level) where
  StringType 'TypeLevel  = Symbol
  StringType 'ValueLevel = String

type family IntegerType (lvl :: Level) where
  IntegerType 'TypeLevel  = Nat
  IntegerType 'ValueLevel = Int

-- | Sudo ID of a property

data Essence (lvl :: Level) where
  Ess :: StringType lvl -> Essence lvl

-- | Value definition

data ValDef (lvl :: Level) where
  IntValue      :: IntegerType lvl -> ValDef lvl
  IntRangeValue :: (IntegerType lvl, IntegerType lvl) -> ValDef lvl
  BoolValue     :: Bool -> ValDef lvl
  PairValue     :: ValDef lvl -> ValDef  lvl-> ValDef lvl
  PropRefValue  :: [Essence lvl] -> ValDef lvl

-- | Variable definition

type VarName (lvl :: Level) = StringType lvl

data VarDef (lvl :: Level) where
  IntVar        :: VarName lvl -> VarDef lvl
  IntRangeVar   :: VarName lvl -> VarDef lvl
  BoolVar       :: VarName lvl -> VarDef lvl
  PairVar       :: VarName lvl -> VarDef lvl -> VarDef lvl -> VarDef lvl

------ Query and Procedure Script ------------

data QuerySetting where
  FollowReferences :: QuerySetting

data CompareOp where
  Eq :: CompareOp

data QueryTerm (lvl :: Level) where
  QEssence :: Essence lvl -> QueryTerm lvl
  QGetEssence :: QueryTerm lvl

type QueryPath (lvl :: Level) = [QueryTerm lvl]

data Query (lvl :: Level) where
  SimpleQuery
    :: [QuerySetting]
    -> QueryPath lvl
    -> VarDef lvl
    -> Query lvl

data Condition (lvl :: Level) where
  ConditionDef
    :: VarName lvl
    -> CompareOp
    -> ValDef lvl
    -> Condition lvl

data Procedure (lvl :: Level) where
  ReplaceProp
    :: [Essence lvl]
    -> [Essence lvl]
    -> Procedure lvl

data Action (lvl :: Level) where
  ConditionalAction
    :: Condition lvl
    -> Procedure lvl
    -> Action lvl

data Script (lvl :: Level) where
  NoScript :: Script lvl    -- TODO: temporary. Remove
  SimpleScript
    :: Essence lvl
    -> [Query lvl]       -- ^ Query specific values before the script
    -> [Action lvl]
    -> Script lvl

------ Property -----

-- | Used to make property hierarchies.
-- Essence arg: own essence
-- Property arg: parent property
data PropertyRoot (lvl :: Level) where
  EssRoot   :: Essence lvl -> PropertyRoot lvl
  PropRoot  :: Essence lvl -> Property lvl -> PropertyRoot lvl

-- | Used to make static property hierarchies.
-- Essence arg: own essence
-- Property arg: parent property
data StaticPropertyRoot (lvl :: Level) where
  EssStaticRoot   :: Essence lvl -> StaticPropertyRoot lvl
  PropStaticRoot  :: Essence lvl -> StaticProperty lvl -> StaticPropertyRoot lvl

-- Property owning replaces materialization from the prev version
data PropertyOwning (lvl :: Level) where
  -- | Property will be materialized for each parent prop.
  OwnProp    :: Property lvl -> PropertyOwning lvl
  -- | Property will be materialized only once and shared between parents.
  SharedProp :: Property lvl -> PropertyOwning lvl

data PropertyKeyValue (lvl :: Level) where
  -- | Implicit dictionary of properties.
  -- When materialized, becomes a dict with keys taken from properties
  PropKeyBag :: Essence lvl -> [PropertyOwning lvl] -> PropertyKeyValue lvl
  -- | Separate property
  PropKeyVal :: Essence lvl -> PropertyOwning lvl -> PropertyKeyValue lvl

data StaticProperty (lvl :: Level) where
  StaticProp
    :: StaticPropertyRoot lvl
    -> StaticProperty lvl

data Property (lvl :: Level) where
  -- | Static prop reference. Will not be materialized.
  StaticPropRef
    :: StaticProperty lvl
    -> Property lvl
  -- | Lear prop. Value will be materialized as const.
  PropConst
    :: PropertyRoot lvl
    -> ValDef lvl
    -> Property lvl
  -- | Lear prop. Value will be materialized as mutable var (TVar).
  PropVal
    :: PropertyRoot lvl
    -> ValDef lvl
    -> Property lvl
  -- | Compound property.
  -- Each prop in the bag is a mutable reference.
  -- Each prop can be replaced by some other prop.
  PropDict
    :: PropertyRoot lvl
    -> [PropertyKeyValue lvl]
    -> Property lvl
  -- | Property script
  PropScript
    :: Essence lvl
    -> Script lvl
    -> Property lvl

