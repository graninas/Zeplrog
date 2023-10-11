{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Common where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization.Materializer

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


-- TODO: FIXME: bare String type

data Essences essPath

-- Statically materialize variable def

instance
  ( KnownSymbol str
  ) =>
  SMat p str String where
  sMat p _ = pure $ symbolVal $ Proxy @str

instance
  ( KnownSymbol varName
  ) =>
  SMat p ('IntVar @'TypeLevel varName) (VarDef 'ValueLevel) where
  sMat p _ = do
    let varName = symbolVal $ Proxy @varName
    pure $ IntVar varName

instance
  ( KnownSymbol varName
  ) =>
  SMat p ('BoolVar @'TypeLevel varName) (VarDef 'ValueLevel) where
  sMat p _ = do
    let varName = symbolVal $ Proxy @varName
    pure $ BoolVar varName

instance
  ( KnownSymbol varName
  , SMat p varDef1 (VarDef 'ValueLevel)
  , SMat p varDef2 (VarDef 'ValueLevel)
  ) =>
  SMat p ('PairVar @'TypeLevel varName varDef1 varDef2)
      (VarDef 'ValueLevel) where
  sMat p _ = do
    let varName = symbolVal $ Proxy @varName
    varDef1 <- sMat p $ Proxy @varDef1
    varDef2 <- sMat p $ Proxy @varDef2
    pure $ PairVar varName varDef1 varDef2

-- Statically materialize value

instance
  ( KnownNat intVal
  ) =>
  SMat p ('IntValue @'TypeLevel intVal)
      (ValDef 'ValueLevel) where
  sMat p _ = pure
      $ IntValue
      $ fromIntegral
      $ natVal
      $ Proxy @intVal

instance
  ( SMat p val1 (ValDef 'ValueLevel)
  , SMat p val2 (ValDef 'ValueLevel)
  ) =>
  SMat p ('PairValue @'TypeLevel val1 val2) (ValDef 'ValueLevel) where
  sMat p _ = do
    val1 <- sMat p $ Proxy @val1
    val2 <- sMat p $ Proxy @val2
    pure $ PairValue val1 val2

instance
  SMat p ('BoolValue @'TypeLevel 'True) (ValDef 'ValueLevel) where
  sMat p _ = pure $ BoolValue True

instance
  SMat p ('BoolValue @'TypeLevel 'False) (ValDef 'ValueLevel) where
  sMat p _ = pure $ BoolValue False

instance
  ( SMat p (Essences essPath) [Essence 'ValueLevel]
  ) =>
  SMat p ('PropRefValue @'TypeLevel essPath)
      (ValDef 'ValueLevel) where
  sMat p _ = do
    path <- sMat p $ Proxy @(Essences essPath)
    pure $ PropRefValue path

-- Statically materialize Essence path

instance
  ( KnownSymbol symb
  ) =>
  SMat p ('Ess @'TypeLevel symb) (Essence 'ValueLevel) where
  sMat p _ = pure $ Ess $ symbolVal (Proxy @symb)

instance
  SMat p (Essences '[]) [Essence 'ValueLevel] where
  sMat p _ = pure []

instance
  ( SMat p ess (Essence 'ValueLevel)
  , SMat p (Essences essPath) [Essence 'ValueLevel]
  ) =>
  SMat p (Essences (ess ': essPath))
      [Essence 'ValueLevel] where
  sMat p _ = do
    ess     <- sMat p $ Proxy @ess
    essPath <- sMat p $ Proxy @(Essences essPath)
    pure $ ess : essPath
