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
  Mat str String where
  mat _ = pure $ symbolVal $ Proxy @str

instance
  ( KnownSymbol varName
  ) =>
  Mat ('IntVar @'TypeLevel varName) (VarDef 'ValueLevel) where
  mat _ = do
    let varName = symbolVal $ Proxy @varName
    pure $ IntVar varName

instance
  ( KnownSymbol varName
  ) =>
  Mat ('BoolVar @'TypeLevel varName) (VarDef 'ValueLevel) where
  mat _ = do
    let varName = symbolVal $ Proxy @varName
    pure $ BoolVar varName

instance
  ( KnownSymbol varName
  , Mat varDef1 (VarDef 'ValueLevel)
  , Mat varDef2 (VarDef 'ValueLevel)
  ) =>
  Mat ('PairVar @'TypeLevel varName varDef1 varDef2)
      (VarDef 'ValueLevel) where
  mat _ = do
    let varName = symbolVal $ Proxy @varName
    varDef1 <- mat $ Proxy @varDef1
    varDef2 <- mat $ Proxy @varDef2
    pure $ PairVar varName varDef1 varDef2


-- Statically materialize value

instance
  ( KnownNat intVal
  ) =>
  Mat ('IntValue @'TypeLevel intVal)
      (ValDef 'ValueLevel) where
  mat _ = pure
      $ IntValue
      $ fromIntegral
      $ natVal
      $ Proxy @intVal

instance
  ( Mat val1 (ValDef 'ValueLevel)
  , Mat val2 (ValDef 'ValueLevel)
  ) =>
  Mat ('PairValue @'TypeLevel val1 val2) (ValDef 'ValueLevel) where
  mat _ = do
    val1 <- mat $ Proxy @val1
    val2 <- mat $ Proxy @val2
    pure $ PairValue val1 val2

instance
  Mat ('BoolValue @'TypeLevel 'True) (ValDef 'ValueLevel) where
  mat _ = pure $ BoolValue True

instance
  Mat ('BoolValue @'TypeLevel 'False) (ValDef 'ValueLevel) where
  mat _ = pure $ BoolValue False

instance
  ( Mat (Essences essPath) [Essence 'ValueLevel]
  ) =>
  Mat ('PropRefValue @'TypeLevel essPath)
      (ValDef 'ValueLevel) where
  mat _ = do
    path <- mat $ Proxy @(Essences essPath)
    pure $ PropRefValue path

-- Statically materialize Essence path

instance
  ( KnownSymbol symb
  ) =>
  Mat ('Ess @'TypeLevel symb) (Essence 'ValueLevel) where
  mat _ = pure $ Ess $ symbolVal (Proxy @symb)

instance
  Mat (Essences '[]) [Essence 'ValueLevel] where
  mat _ = pure []

instance
  ( Mat ess (Essence 'ValueLevel)
  , Mat (Essences essPath) [Essence 'ValueLevel]
  ) =>
  Mat (Essences (ess ': essPath))
      [Essence 'ValueLevel] where
  mat _ = do
    ess     <- mat $ Proxy @ess
    essPath <- mat $ Proxy @(Essences essPath)
    pure $ ess : essPath
