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
  Mat 'FollowReferences QuerySetting where
  mat _ = pure FollowReferences

instance
  Mat (Settings '[]) [QuerySetting] where
  mat _ = pure []

instance
  ( Mat setting QuerySetting
  , Mat (Settings settings) [QuerySetting]
  ) =>
  Mat (Settings (setting ': settings))
      [QuerySetting] where
  mat _ = do
    setting  <- mat $ Proxy @setting
    settings <- mat $ Proxy @(Settings settings)
    pure $ setting : settings

-- Statically materialize query term

instance
  ( Mat ess (Essence 'ValueLevel)
  ) =>
  Mat ('QEssence @'TypeLevel ess) (QueryTerm 'ValueLevel) where
  mat _ = do
    ess <- mat $ Proxy @ess
    pure $ QEssence ess

instance
  Mat ('QGetEssence @'TypeLevel) (QueryTerm 'ValueLevel) where
  mat _ = pure QGetEssence

-- Statically materialize query path

instance
  Mat (QPathItems '[]) [QueryTerm 'ValueLevel] where
  mat _ = pure []

instance
  ( Mat item (QueryTerm 'ValueLevel)
  , Mat (QPathItems items) [QueryTerm 'ValueLevel]
  ) =>
  Mat (QPathItems (item ': items))
      [QueryTerm 'ValueLevel] where
  mat _ = do
    item  <- mat $ Proxy @item
    items <- mat $ Proxy @(QPathItems items)
    pure $ item : items

-- Statically materialize query

instance
  ( Mat (Settings settings) [QuerySetting]
  , Mat (QPathItems qPath) [QueryTerm 'ValueLevel]
  , Mat varDef (VarDef 'ValueLevel)
  ) =>
  Mat ('SimpleQuery @'TypeLevel settings qPath varDef) (Query 'ValueLevel) where
  mat _ = do
    settings <- mat $ Proxy @(Settings settings)
    qPath    <- mat $ Proxy @(QPathItems qPath)
    varDef   <- mat $ Proxy @varDef
    pure $ SimpleQuery settings qPath varDef

-- Statically materialize queries

instance
  Mat (Queries '[]) [Query 'ValueLevel] where
  mat _ = pure []

instance
  ( Mat q (Query 'ValueLevel)
  , Mat (Queries qs) [Query 'ValueLevel]
  ) =>
  Mat (Queries (q ': qs))
      [Query 'ValueLevel] where
  mat _ = do
    q     <- mat $ Proxy @q
    qs <- mat $ Proxy @(Queries qs)
    pure $ q : qs

-- Statically materialize compare op

instance
  Mat 'QEq CompareOp where
  mat _ = pure QEq

-- Statically materialize procedure

instance
  ( Mat (Essences whatProp) [Essence 'ValueLevel]
  , Mat (Essences withProp) [Essence 'ValueLevel]
  ) =>
  Mat ('ReplaceProp @'TypeLevel whatProp withProp) (Procedure 'ValueLevel) where
  mat _ = do
    whatPropPath <- mat $ Proxy @(Essences whatProp)
    withPropPath <- mat $ Proxy @(Essences withProp)
    pure $ ReplaceProp whatPropPath withPropPath

-- Statically materialize condition

instance
  ( Mat varName (VarName 'ValueLevel)
  , Mat op CompareOp
  , Mat valDef (ValDef 'ValueLevel)
  ) =>
  Mat ('ConditionDef @'TypeLevel varName op valDef) (Condition 'ValueLevel) where
  mat _ = do
    varName <- mat $ Proxy @varName
    op      <- mat $ Proxy @op
    valDef  <- mat $ Proxy @valDef
    pure $ ConditionDef varName op valDef

-- Statically materialize action

instance
  ( Mat cond (Condition 'ValueLevel)
  , Mat procedure (Procedure 'ValueLevel)
  ) =>
  Mat ('ConditionalAction @'TypeLevel cond procedure) (Action 'ValueLevel) where
  mat _ = do
    cond      <- mat $ Proxy @cond
    procedure <- mat $ Proxy @procedure
    pure $ ConditionalAction cond procedure

-- Statically materialize actions

instance
  Mat (Actions '[]) [Action 'ValueLevel] where
  mat _ = pure []

instance
  ( Mat act (Action 'ValueLevel)
  , Mat (Actions acts) [Action 'ValueLevel]
  ) =>
  Mat (Actions (act ': acts))
      [Action 'ValueLevel] where
  mat _ = do
    act  <- mat $ Proxy @act
    acts <- mat $ Proxy @(Actions acts)
    pure $ act : acts

-- Statically materialize script

instance
  ( Mat ess (Essence 'ValueLevel)
  , Mat (Queries queries) [Query 'ValueLevel]
  , Mat (Actions actions) [Action 'ValueLevel]
  ) =>
  Mat ('SimpleScript @'TypeLevel ess queries actions) (Script 'ValueLevel) where
  mat _ = do
    ess     <- mat $ Proxy @ess
    queries <- mat $ Proxy @(Queries queries)
    actions <- mat $ Proxy @(Actions actions)
    pure $ SimpleScript ess queries actions
