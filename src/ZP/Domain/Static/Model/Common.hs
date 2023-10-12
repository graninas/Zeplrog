{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZP.Domain.Static.Model.Common where

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
  StringType 'ValueLevel = String     -- TODO: use Text

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
  StringValue   :: StringType lvl-> ValDef lvl

  -- Should be dyn materialized as a mutable reference to a property
  -- (list of essences relative to the parent property)
  PropRefValue  :: [Essence lvl] -> ValDef lvl

-- | Variable definition

type VarName (lvl :: Level) = StringType lvl

data VarDef (lvl :: Level) where
  IntVar        :: VarName lvl -> VarDef lvl
  BoolVar       :: VarName lvl -> VarDef lvl
  PairVar       :: VarName lvl -> VarDef lvl -> VarDef lvl -> VarDef lvl

------ Short identifiers ----------

type EssenceTL = Essence 'TypeLevel
type EssenceVL = Essence 'ValueLevel

type ValDefTL = ValDef 'TypeLevel
type ValDefVL = ValDef 'ValueLevel

type VarDefTL = VarDef 'TypeLevel
type VarDefVL = VarDef 'ValueLevel
