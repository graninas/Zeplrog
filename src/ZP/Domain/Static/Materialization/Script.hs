{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ZP.Domain.Static.Materialization.Script where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization.Materializer
import ZP.Domain.Static.Materialization.Common

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map



data ScrOps ops

-- Var materialization

instance
  ( KnownSymbol varName
  , SMat () defVal (GenericValDefVL tag)
  ) =>
  SMat (Proxy tag)
        ('GenericVar @'TypeLevel varName defVal)
        (GenericVarDefVL tag) where
  sMat _ _ = do
    let varName = symbolVal $ Proxy @varName
    val <- sMat () $ Proxy @defVal
    pure $ GenericVar varName val

-- Target materialization

instance
  ( varDef ~ (vd :: GenericVarDefTL tag)
  , SMat (Proxy tag) varDef (GenericVarDefVL tag)
  ) =>
  SMat () ('ToVar @'TypeLevel varDef)
          (TargetVL tag) where
  sMat () _ = do
    varDef <- sMat (Proxy @tag) $ Proxy @varDef
    pure $ ToVar varDef

instance
  ( SMat () path EssencePathVL
  , proxy ~ (p :: Proxy tag)
  ) =>
  SMat () ('ToField proxy path)
          (TargetVL tag) where
  sMat () _ = do
    path <- sMat () $ Proxy @path
    pure $ ToField (Proxy @tag) path

instance
  ( varDef ~ (vd :: GenericVarDefTL tag)
  , SMat (Proxy tag) varDef (GenericVarDefVL tag)
  ) =>
  SMat () ('FromVar @'TypeLevel varDef)
          (SourceVL tag) where
  sMat () _ = do
    varDef <- sMat (Proxy @tag) $ Proxy @varDef
    pure $ FromVar varDef

instance
  ( SMat () path EssencePathVL
  , proxy ~ (p :: Proxy tag)
  ) =>
  SMat () ('FromField proxy path)
          (SourceVL tag) where
  sMat () _ = do
    path <- sMat () $ Proxy @path
    pure $ FromField (Proxy @tag) path

instance
  ( constDef ~ (c :: GenericConstDefTL tag)
  , SMat (Proxy tag) constDef (GenericConstDefVL tag)
  ) =>
  SMat () ('FromConst constDef)
          (SourceVL tag) where
  sMat () _ = do
    val <- sMat (Proxy @tag) $ Proxy @constDef
    pure $ FromConst val

instance
  SMat () ('NegateF @'TypeLevel)
          (FuncVL BoolTag BoolTag) where
  sMat () _ = pure NegateF

instance
  ( SMat (Proxy tag) varDef (GenericVarDefVL tag)
  ) =>
  SMat () ('DeclareVar @'TypeLevel varDef)
         ScriptOpVL where
  sMat () _ = do
    varDef <- sMat (Proxy @tag) $ Proxy @varDef
    pure $ DeclareVar varDef

instance
  ( SMat () source (SourceVL tag)
  , SMat () target (TargetVL tag)
  ) =>
  SMat () ('WriteData @'TypeLevel target source)
         ScriptOpVL where
  sMat () _ = do
    source <- sMat () $ Proxy @source
    target <- sMat () $ Proxy @target
    pure $ WriteData target source

instance
  ( SMat () func   (FuncVL tag1 tag2)
  , SMat () source (SourceVL tag1)
  , SMat () target (TargetVL tag2)
  ) =>
  SMat () ('Invoke @'TypeLevel func source target)
         ScriptOpVL where
  sMat () _ = do
    func   <- sMat () $ Proxy @func
    source <- sMat () $ Proxy @source
    target <- sMat () $ Proxy @target
    pure $ Invoke func source target

instance
  SMat () (ScrOps '[]) [ScriptOpVL] where
  sMat () _ = pure []

instance
  ( SMat () op ScriptOpVL
  , SMat () (ScrOps ops) [ScriptOpVL]
  ) =>
  SMat () (ScrOps (op ': ops)) [ScriptOpVL] where
  sMat () _ = do
    op  <- sMat () $ Proxy @op
    ops <- sMat () $ Proxy @(ScrOps ops)
    pure $ op : ops

instance
  ( KnownSymbol descr
  , SMat () (ScrOps ops) [ScriptOpVL]
  ) =>
  SMat () ('Script @'TypeLevel descr ops)
         CustomScriptVL where
  sMat () _ = do
    let descr = symbolVal $ Proxy @descr
    ops   <- sMat () $ Proxy @(ScrOps ops)
    pure $ Script descr ops


