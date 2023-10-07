{-# LANGUAGE DataKinds #-}

module ZP.Domain.Static.Model where

import ZP.Prelude
import GHC.TypeLits

------ Common and General -----------------

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
  IntValDef      :: IntegerType lvl -> ValDef lvl
  IntRangeValDef :: (IntegerType lvl, IntegerType lvl) -> ValDef lvl
  BoolValDef     :: Bool -> ValDef lvl
  PairValDef     :: ValDef lvl -> ValDef  lvl-> ValDef lvl

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
    -> Property lvl
    -> Procedure lvl

data Action (lvl :: Level) where
  ConditionalAction
    :: Condition lvl
    -> Procedure lvl
    -> Action lvl

data Script (lvl :: Level) where
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


type Category (lvl :: Level) = Essence lvl

data PropertyKeyValue (lvl :: Level) where
  -- | Implicit dictionary of properties.
  -- When materialized, becomes a dict with keys taken from properties
  PropKeyBag :: Category lvl -> [PropertyOwning lvl] -> PropertyKeyValue lvl
  -- | Separate property
  PropKeyVal :: Category lvl -> PropertyOwning lvl -> PropertyKeyValue lvl

data StaticProperty (lvl :: Level) where
  StaticProp :: StaticPropertyRoot lvl -> StaticProperty lvl

data Property (lvl :: Level) where
  -- | Static prop reference. Will not be materialized.
  StaticPropRef
    :: StaticProperty lvl
    -> Property lvl
  -- | Leaf prop. Points to another prop with an essence path.
  --   Will be materialized as a const.
  PropRef
    :: [Essence lvl]
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
    :: Script lvl
    -> Property lvl




