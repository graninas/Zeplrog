{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZP.Domain.Static.Model.CommonOld where

import ZP.Prelude
import GHC.TypeLits

import qualified Text.Show as T

-- ------ Common and General -----------------

-- -- | Specific mechanism (a kind of the HKD pattern)
-- -- to choose between Symbol and String, Nat and Int
-- -- when using the model either as type-level or as value-level one.
-- -- Very boilerplaity. Has a high accidental complexity.
-- -- Alternative: a completely separate yet almost identical
-- -- of set of types for the static value-level model
-- -- with Strings instead Symbols and Ints instead Nats.

-- -- | Sudo ID of a property

-- data Essence (lvl :: Level) where
--   Ess :: StringType lvl -> Essence lvl

-- type EssencePath (lvl :: Level) = [Essence lvl]

-- -- | Real Id of a property (for value-level usage only)

-- newtype StaticPropertyId = StaticPropertyId Int
--   deriving (Show, Eq, Ord)

-- -- | Value definition

-- data ValDef (lvl :: Level) where
--   IntValue      :: IntegerType lvl -> ValDef lvl
--   BoolValue     :: Bool -> ValDef lvl
--   PairValue     :: ValDef lvl -> ValDef  lvl-> ValDef lvl
--   StringValue   :: StringType lvl-> ValDef lvl

--   -- | Reference to a dynamic property relative to the parent prop
--   PathValue     :: [Essence lvl] -> ValDef lvl

--   -- Special value definitions.

--   -- | Random int value.
--   --   Gets randomized value when materialized dynamically.
--   RandomIntValue :: IntegerType lvl -> IntegerType lvl -> ValDef lvl

--   -- | Derived position in the world.
--   --   Equivalent to abstract function.

--   --   For abstract static property, indicates that the derived property
--   --     should contain this value.

--   --   For a concrete static property, indicates that the value
--   --     should be instantiated at dynamic materialization.

--   --   Should not be present in a dynamic property.
--   DerivedWorldPos :: ValDef lvl

-- -- | Variable definition

-- type VarName (lvl :: Level) = StringType lvl

-- data VarDef (lvl :: Level) where
--   IntVar        :: VarName lvl -> VarDef lvl
--   BoolVar       :: VarName lvl -> VarDef lvl
--   PairVar       :: VarName lvl -> VarDef lvl -> VarDef lvl -> VarDef lvl

-- ------ Short identifiers ----------

-- type EssenceTL = Essence 'TypeLevel
-- type EssenceVL = Essence 'ValueLevel

-- type ValDefTL = ValDef 'TypeLevel
-- type ValDefVL = ValDef 'ValueLevel

-- type VarDefTL = VarDef 'TypeLevel
-- type VarDefVL = VarDef 'ValueLevel

-- type EssencePathTL = EssencePath 'TypeLevel
-- type EssencePathVL = EssencePath 'ValueLevel

-- -------- Instances ------------------

-- instance Eq EssenceVL where
--   (==) (Ess a) (Ess b) = a == b

-- instance Ord EssenceVL where
--   compare (Ess a) (Ess b) = compare a b

-- instance T.Show EssenceVL where
--   show (Ess a) = T.show a
