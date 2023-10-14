{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Script where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization.Materializer
import ZP.Domain.Static.Materialization.Common

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


---------- Script --------------

data Queries qs
data Settings sets
data Actions acts
data QPathItems items

-- Statically materialize settings

instance
  SMat () 'FollowReferences QuerySetting where
  sMat () _ = pure FollowReferences

instance
  SMat () (Settings '[]) [QuerySetting] where
  sMat () _ = pure []

instance
  ( SMat () setting QuerySetting
  , SMat () (Settings settings) [QuerySetting]
  ) =>
  SMat () (Settings (setting ': settings))
      [QuerySetting] where
  sMat () _ = do
    setting  <- sMat () $ Proxy @setting
    settings <- sMat () $ Proxy @(Settings settings)
    pure $ setting : settings

-- Statically materialize query term

instance
  ( SMat () ess (Essence 'ValueLevel)
  ) =>
  SMat () ('QEssence @'TypeLevel ess) (QueryTerm 'ValueLevel) where
  sMat () _ = do
    ess <- sMat () $ Proxy @ess
    pure $ QEssence ess

instance
  SMat () ('QGetEssence @'TypeLevel) (QueryTerm 'ValueLevel) where
  sMat () _ = pure QGetEssence

-- Statically materialize query path

instance
  SMat () (QPathItems '[]) [QueryTerm 'ValueLevel] where
  sMat () _ = pure []

instance
  ( SMat () item (QueryTerm 'ValueLevel)
  , SMat () (QPathItems items) [QueryTerm 'ValueLevel]
  ) =>
  SMat () (QPathItems (item ': items))
      [QueryTerm 'ValueLevel] where
  sMat () _ = do
    item  <- sMat () $ Proxy @item
    items <- sMat () $ Proxy @(QPathItems items)
    pure $ item : items

-- Statically materialize query

instance
  ( SMat () (Settings settings) [QuerySetting]
  , SMat () (QPathItems qPath) [QueryTerm 'ValueLevel]
  , SMat () varDef (VarDef 'ValueLevel)
  ) =>
  SMat () ('SimpleQuery @'TypeLevel settings qPath varDef) (Query 'ValueLevel) where
  sMat () _ = do
    settings <- sMat () $ Proxy @(Settings settings)
    qPath    <- sMat () $ Proxy @(QPathItems qPath)
    varDef   <- sMat () $ Proxy @varDef
    pure $ SimpleQuery settings qPath varDef

-- Statically materialize queries

instance
  SMat () (Queries '[]) [Query 'ValueLevel] where
  sMat () _ = pure []

instance
  ( SMat () q (Query 'ValueLevel)
  , SMat () (Queries qs) [Query 'ValueLevel]
  ) =>
  SMat () (Queries (q ': qs))
      [Query 'ValueLevel] where
  sMat () _ = do
    q     <- sMat () $ Proxy @q
    qs <- sMat () $ Proxy @(Queries qs)
    pure $ q : qs

-- Statically materialize compare op

instance
  SMat () 'QEq CompareOp where
  sMat () _ = pure QEq

-- Statically materialize procedure

instance
  ( SMat () (Essences whatProp) [Essence 'ValueLevel]
  , SMat () (Essences withProp) [Essence 'ValueLevel]
  ) =>
  SMat () ('ReplaceProp @'TypeLevel whatProp withProp) (Procedure 'ValueLevel) where
  sMat () _ = do
    whatPropPath <- sMat () $ Proxy @(Essences whatProp)
    withPropPath <- sMat () $ Proxy @(Essences withProp)
    pure $ ReplaceProp whatPropPath withPropPath

-- Statically materialize condition

instance
  ( SMat () varName (VarName 'ValueLevel)
  , SMat () op CompareOp
  , SMat () valDef (ValDef 'ValueLevel)
  ) =>
  SMat () ('ConditionDef @'TypeLevel varName op valDef) (Condition 'ValueLevel) where
  sMat () _ = do
    varName <- sMat () $ Proxy @varName
    op      <- sMat () $ Proxy @op
    valDef  <- sMat () $ Proxy @valDef
    pure $ ConditionDef varName op valDef

-- Statically materialize action

instance
  ( SMat () cond (Condition 'ValueLevel)
  , SMat () procedure (Procedure 'ValueLevel)
  ) =>
  SMat () ('ConditionalAction @'TypeLevel cond procedure) (Action 'ValueLevel) where
  sMat () _ = do
    cond      <- sMat () $ Proxy @cond
    procedure <- sMat () $ Proxy @procedure
    pure $ ConditionalAction cond procedure

-- Statically materialize actions

instance
  SMat () (Actions '[]) [Action 'ValueLevel] where
  sMat () _ = pure []

instance
  ( SMat () act (Action 'ValueLevel)
  , SMat () (Actions acts) [Action 'ValueLevel]
  ) =>
  SMat () (Actions (act ': acts))
      [Action 'ValueLevel] where
  sMat () _ = do
    act  <- sMat () $ Proxy @act
    acts <- sMat () $ Proxy @(Actions acts)
    pure $ act : acts

-- Statically materialize script

instance
  ( SMat () ess (Essence 'ValueLevel)
  , SMat () (Queries queries) [Query 'ValueLevel]
  , SMat () (Actions actions) [Action 'ValueLevel]
  ) =>
  SMat () ('SimpleScript @'TypeLevel ess queries actions) (Script 'ValueLevel) where
  sMat () _ = do
    ess     <- sMat () $ Proxy @ess
    queries <- sMat () $ Proxy @(Queries queries)
    actions <- sMat () $ Proxy @(Actions actions)
    pure $ SimpleScript ess queries actions
