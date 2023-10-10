{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZP.Domain.Static.Model where

import ZP.Prelude
import GHC.TypeLits

import qualified Text.Show as T

------ Common and General -----------------

-- | Specific mechanism (a kind of the HKD pattern)
-- to choose between Symbol and String, Nat and Int
-- when using the model either as type-level or as value-level one.
-- Very boilerplaity. Has a high accidental complexity.
-- Alternative: a completely separate yet almost identical
-- of set of types for the static value-level model
-- with Strings instead Symbols and Ints instead Nats.

data Level = TypeLevel | ValueLevel
  deriving (Show, Eq, Ord)

type family StringType (lvl :: Level) where
  StringType 'TypeLevel  = Symbol
  StringType 'ValueLevel = String

type family IntegerType (lvl :: Level) where
  IntegerType 'TypeLevel  = Nat
  IntegerType 'ValueLevel = Int

-- | Sudo ID of a property

data Essence (lvl :: Level) where
  Ess :: StringType lvl -> Essence lvl

instance Eq (Essence 'ValueLevel) where
  (==) (Ess a) (Ess b) = a == b

instance Ord (Essence 'ValueLevel) where
  compare (Ess a) (Ess b) = compare a b

instance T.Show (Essence 'ValueLevel) where
  show (Ess a) = T.show a

-- | Value definition

data ValDef (lvl :: Level) where
  IntValue      :: IntegerType lvl -> ValDef lvl
  BoolValue     :: Bool -> ValDef lvl
  PairValue     :: ValDef lvl -> ValDef  lvl-> ValDef lvl

  -- Should be dyn materialized as a mutable reference to a property
  -- (list of essences relative to the parent property)
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
  QEq :: CompareOp

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

-- | Used to make static property hierarchies.
-- Essence arg: own essence
-- Property arg: parent property
data StaticPropertyRoot (lvl :: Level) where
  EssStaticRoot   :: Essence lvl -> StaticPropertyRoot lvl
  PropStaticRoot  :: Essence lvl -> Property lvl -> StaticPropertyRoot lvl

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

-- | Static property that must be stat and dyn materialized.
data Property (lvl :: Level) where
  -- | Static prop. Referenced prop can't be dyn materialized.
  StaticProp
    :: StaticPropertyRoot lvl
    -> Property lvl
  -- | Static prop reference. The referenced prop can't be dyn materialized.
  StaticPropRef
    :: Property lvl
    -> Property lvl
  -- | Lear prop. Value will be dyn materialized as const.
  PropConst
    :: StaticPropertyRoot lvl
    -> ValDef lvl
    -> Property lvl
  -- | Lear prop. Value will be dyn materialized as mutable var (TVar).
  PropVal
    :: StaticPropertyRoot lvl
    -> ValDef lvl
    -> Property lvl
  -- | Compound property.
  -- Each prop in the bag is a mutable reference.
  -- Each prop can be replaced by some other prop in the dyn model.
  PropDict
    :: StaticPropertyRoot lvl
    -> [PropertyKeyValue lvl]
    -> Property lvl
  -- | Property script.
  PropScript
    :: StaticPropertyRoot lvl
    -> Script lvl
    -> Property lvl

