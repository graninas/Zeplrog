{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZP.Domain.Static.Model.Script where

import ZP.Prelude
import GHC.TypeLits
import qualified Text.Show as T

import ZP.Domain.Static.Model.Common

------ Query and Procedure Script ------------

data QuerySetting where
  FollowReferences :: QuerySetting

data CompareOp where
  QEq :: CompareOp

data QueryTerm (lvl :: Level) where
  QEssence    :: Essence lvl -> QueryTerm lvl
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
  SimpleScript
    :: Essence lvl
    -> [Query lvl]       -- ^ Query specific values before the script
    -> [Action lvl]
    -> Script lvl

