{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Common where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization.Materializer

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


data Essences essPath

-- Statically materialize variable def

instance
  ( KnownSymbol str
  ) =>
  SMat str String where
  sMat _ = pure $ symbolVal $ Proxy @str

instance
  ( KnownSymbol varName
  ) =>
  SMat ('IntVar @'TypeLevel varName) (VarDef 'ValueLevel) where
  sMat _ = do
    let varName = symbolVal $ Proxy @varName
    pure $ IntVar varName

instance
  ( KnownSymbol varName
  ) =>
  SMat ('BoolVar @'TypeLevel varName) (VarDef 'ValueLevel) where
  sMat _ = do
    let varName = symbolVal $ Proxy @varName
    pure $ BoolVar varName

instance
  ( KnownSymbol varName
  , SMat varDef1 (VarDef 'ValueLevel)
  , SMat varDef2 (VarDef 'ValueLevel)
  ) =>
  SMat ('PairVar @'TypeLevel varName varDef1 varDef2)
      (VarDef 'ValueLevel) where
  sMat _ = do
    let varName = symbolVal $ Proxy @varName
    varDef1 <- sMat $ Proxy @varDef1
    varDef2 <- sMat $ Proxy @varDef2
    pure $ PairVar varName varDef1 varDef2

-- Statically materialize value

instance
  ( KnownNat intVal
  ) =>
  SMat ('IntValue @'TypeLevel intVal)
      (ValDef 'ValueLevel) where
  sMat _ = pure
      $ IntValue
      $ fromIntegral
      $ natVal
      $ Proxy @intVal

instance
  ( SMat val1 (ValDef 'ValueLevel)
  , SMat val2 (ValDef 'ValueLevel)
  ) =>
  SMat ('PairValue @'TypeLevel val1 val2) (ValDef 'ValueLevel) where
  sMat _ = do
    val1 <- sMat $ Proxy @val1
    val2 <- sMat $ Proxy @val2
    pure $ PairValue val1 val2

instance
  SMat ('BoolValue @'TypeLevel 'True) (ValDef 'ValueLevel) where
  sMat _ = pure $ BoolValue True

instance
  SMat ('BoolValue @'TypeLevel 'False) (ValDef 'ValueLevel) where
  sMat _ = pure $ BoolValue False

instance
  ( SMat (Essences essPath) [Essence 'ValueLevel]
  ) =>
  SMat ('PropRefValue @'TypeLevel essPath)
      (ValDef 'ValueLevel) where
  sMat _ = do
    path <- sMat $ Proxy @(Essences essPath)
    pure $ PropRefValue path

-- Statically materialize Essence path

instance
  ( KnownSymbol symb
  ) =>
  SMat ('Ess @'TypeLevel symb) (Essence 'ValueLevel) where
  sMat _ = pure $ Ess $ symbolVal (Proxy @symb)

instance
  SMat (Essences '[]) [Essence 'ValueLevel] where
  sMat _ = pure []

instance
  ( SMat ess (Essence 'ValueLevel)
  , SMat (Essences essPath) [Essence 'ValueLevel]
  ) =>
  SMat (Essences (ess ': essPath))
      [Essence 'ValueLevel] where
  sMat _ = do
    ess     <- sMat $ Proxy @ess
    essPath <- sMat $ Proxy @(Essences essPath)
    pure $ ess : essPath
