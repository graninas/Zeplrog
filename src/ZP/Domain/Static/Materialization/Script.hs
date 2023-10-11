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
  SMat p 'FollowReferences QuerySetting where
  sMat p _ = pure FollowReferences

instance
  SMat p (Settings '[]) [QuerySetting] where
  sMat p _ = pure []

instance
  ( SMat p setting QuerySetting
  , SMat p (Settings settings) [QuerySetting]
  ) =>
  SMat p (Settings (setting ': settings))
      [QuerySetting] where
  sMat p _ = do
    setting  <- sMat p $ Proxy @setting
    settings <- sMat p $ Proxy @(Settings settings)
    pure $ setting : settings

-- Statically materialize query term

instance
  ( SMat p ess (Essence 'ValueLevel)
  ) =>
  SMat p ('QEssence @'TypeLevel ess) (QueryTerm 'ValueLevel) where
  sMat p _ = do
    ess <- sMat p $ Proxy @ess
    pure $ QEssence ess

instance
  SMat p ('QGetEssence @'TypeLevel) (QueryTerm 'ValueLevel) where
  sMat p _ = pure QGetEssence

-- Statically materialize query path

instance
  SMat p (QPathItems '[]) [QueryTerm 'ValueLevel] where
  sMat p _ = pure []

instance
  ( SMat p item (QueryTerm 'ValueLevel)
  , SMat p (QPathItems items) [QueryTerm 'ValueLevel]
  ) =>
  SMat p (QPathItems (item ': items))
      [QueryTerm 'ValueLevel] where
  sMat p _ = do
    item  <- sMat p $ Proxy @item
    items <- sMat p $ Proxy @(QPathItems items)
    pure $ item : items

-- Statically materialize query

instance
  ( SMat p (Settings settings) [QuerySetting]
  , SMat p (QPathItems qPath) [QueryTerm 'ValueLevel]
  , SMat p varDef (VarDef 'ValueLevel)
  ) =>
  SMat p ('SimpleQuery @'TypeLevel settings qPath varDef) (Query 'ValueLevel) where
  sMat p _ = do
    settings <- sMat p $ Proxy @(Settings settings)
    qPath    <- sMat p $ Proxy @(QPathItems qPath)
    varDef   <- sMat p $ Proxy @varDef
    pure $ SimpleQuery settings qPath varDef

-- Statically materialize queries

instance
  SMat p (Queries '[]) [Query 'ValueLevel] where
  sMat p _ = pure []

instance
  ( SMat p q (Query 'ValueLevel)
  , SMat p (Queries qs) [Query 'ValueLevel]
  ) =>
  SMat p (Queries (q ': qs))
      [Query 'ValueLevel] where
  sMat p _ = do
    q     <- sMat p $ Proxy @q
    qs <- sMat p $ Proxy @(Queries qs)
    pure $ q : qs

-- Statically materialize compare op

instance
  SMat p 'QEq CompareOp where
  sMat p _ = pure QEq

-- Statically materialize procedure

instance
  ( SMat p (Essences whatProp) [Essence 'ValueLevel]
  , SMat p (Essences withProp) [Essence 'ValueLevel]
  ) =>
  SMat p ('ReplaceProp @'TypeLevel whatProp withProp) (Procedure 'ValueLevel) where
  sMat p _ = do
    whatPropPath <- sMat p $ Proxy @(Essences whatProp)
    withPropPath <- sMat p $ Proxy @(Essences withProp)
    pure $ ReplaceProp whatPropPath withPropPath

-- Statically materialize condition

instance
  ( SMat p varName (VarName 'ValueLevel)
  , SMat p op CompareOp
  , SMat p valDef (ValDef 'ValueLevel)
  ) =>
  SMat p ('ConditionDef @'TypeLevel varName op valDef) (Condition 'ValueLevel) where
  sMat p _ = do
    varName <- sMat p $ Proxy @varName
    op      <- sMat p $ Proxy @op
    valDef  <- sMat p $ Proxy @valDef
    pure $ ConditionDef varName op valDef

-- Statically materialize action

instance
  ( SMat p cond (Condition 'ValueLevel)
  , SMat p procedure (Procedure 'ValueLevel)
  ) =>
  SMat p ('ConditionalAction @'TypeLevel cond procedure) (Action 'ValueLevel) where
  sMat p _ = do
    cond      <- sMat p $ Proxy @cond
    procedure <- sMat p $ Proxy @procedure
    pure $ ConditionalAction cond procedure

-- Statically materialize actions

instance
  SMat p (Actions '[]) [Action 'ValueLevel] where
  sMat p _ = pure []

instance
  ( SMat p act (Action 'ValueLevel)
  , SMat p (Actions acts) [Action 'ValueLevel]
  ) =>
  SMat p (Actions (act ': acts))
      [Action 'ValueLevel] where
  sMat p _ = do
    act  <- sMat p $ Proxy @act
    acts <- sMat p $ Proxy @(Actions acts)
    pure $ act : acts

-- Statically materialize script

instance
  ( SMat p ess (Essence 'ValueLevel)
  , SMat p (Queries queries) [Query 'ValueLevel]
  , SMat p (Actions actions) [Action 'ValueLevel]
  ) =>
  SMat p ('SimpleScript @'TypeLevel ess queries actions) (Script 'ValueLevel) where
  sMat p _ = do
    ess     <- sMat p $ Proxy @ess
    queries <- sMat p $ Proxy @(Queries queries)
    actions <- sMat p $ Proxy @(Actions actions)
    pure $ SimpleScript ess queries actions
