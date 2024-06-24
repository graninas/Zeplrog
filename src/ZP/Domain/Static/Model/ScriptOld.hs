{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZP.Domain.Static.Model.ScriptOld where

-- import ZP.Prelude
-- import GHC.TypeLits
-- import qualified Text.Show as T

-- import ZP.Domain.Static.Model.Common

-- ------ Query and Procedure Script ------------

-- -- | Query settings.

-- data QuerySetting where
--   FollowReferences :: QuerySetting

-- -- | Comparing operators.

-- data CompareOp where
--   QEq :: CompareOp

-- -- | Query path chain.

-- data QueryTerm (lvl :: Level) where
--   QEssence    :: Essence lvl -> QueryTerm lvl
--   QGetEssence :: QueryTerm lvl

-- -- | Path to a value to extract.

-- type QueryPath (lvl :: Level) = [QueryTerm lvl]

-- -- | Query language for extracting values.

-- data Query (lvl :: Level) where
--   SimpleQuery
--     :: [QuerySetting]
--     -> QueryPath lvl
--     -> VarDef lvl
--     -> Query lvl

-- -- | Condition.

-- data Condition (lvl :: Level) where
--   ConditionDef
--     :: VarName lvl
--     -> CompareOp
--     -> ValDef lvl
--     -> Condition lvl

-- -- | Specific procedure for modifying a property.

-- data Procedure (lvl :: Level) where
--   ReplaceProp
--     :: [Essence lvl]
--     -> [Essence lvl]
--     -> Procedure lvl

-- -- | Action to perform over the property when condition is met.

-- data Action (lvl :: Level) where
--   ConditionalAction
--     :: Condition lvl
--     -> Procedure lvl
--     -> Action lvl

-- -- | Script for manipulations with properties.
-- -- For static properties, it allows to query values.
-- -- For dynamic properties, it allows to query values
-- --   and to change variables and properties themselves.

-- data Script (lvl :: Level) where
--   SimpleScript
--     :: Essence lvl
--     -> [Query lvl]       -- ^ Query specific values before the script
--     -> [Action lvl]
--     -> Script lvl

-- ------ Short identifiers ----------

-- type ScriptTL = Script 'TypeLevel
-- type ScriptVL = Script 'ValueLevel
