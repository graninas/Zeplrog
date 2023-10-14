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
  SMat p ('IntVar @'TypeLevel varName) VarDefVL where
  sMat p _ = do
    let varName = symbolVal $ Proxy @varName
    pure $ IntVar varName

instance
  ( KnownSymbol varName
  ) =>
  SMat p ('BoolVar @'TypeLevel varName) VarDefVL where
  sMat p _ = do
    let varName = symbolVal $ Proxy @varName
    pure $ BoolVar varName

instance
  ( KnownSymbol varName
  , SMat p varDef1 VarDefVL
  , SMat p varDef2 VarDefVL
  ) =>
  SMat p ('PairVar @'TypeLevel varName varDef1 varDef2)
      VarDefVL where
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
      ValDefVL where
  sMat p _ = pure
      $ IntValue
      $ fromIntegral
      $ natVal
      $ Proxy @intVal

instance
  ( SMat p val1 ValDefVL
  , SMat p val2 ValDefVL
  ) =>
  SMat p ('PairValue @'TypeLevel val1 val2) ValDefVL where
  sMat p _ = do
    val1 <- sMat p $ Proxy @val1
    val2 <- sMat p $ Proxy @val2
    pure $ PairValue val1 val2

instance
  SMat p ('BoolValue @'TypeLevel 'True) ValDefVL where
  sMat p _ = pure $ BoolValue True

instance
  ( KnownSymbol str
  ) =>
  SMat p ('StringValue @'TypeLevel str) ValDefVL where
  sMat p _ = pure $ StringValue $ symbolVal $ Proxy @str

instance
  SMat p ('BoolValue @'TypeLevel 'False) ValDefVL where
  sMat p _ = pure $ BoolValue False

instance
  ( SMat p (Essences essPath) [EssenceVL]
  ) =>
  SMat p ('PathValue @'TypeLevel essPath)
         ValDefVL where
  sMat p _ = do
    path <- sMat p $ Proxy @(Essences essPath)
    pure $ PathValue path

-- special values

instance
  ( KnownNat from
  , KnownNat to
  ) =>
  SMat p ('RandomIntValue @'TypeLevel from to)
         ValDefVL where
  sMat p _ = pure $ RandomIntValue
    (fromIntegral $ natVal $ Proxy @from)
    (fromIntegral $ natVal $ Proxy @to)

instance
  SMat (Int, Int) ('DerivedWorldPos @'TypeLevel)
       ValDefVL where
  sMat (x, y) _ = pure $ PairValue (IntValue x) (IntValue y)

-- Statically materialize Essence path

instance
  ( KnownSymbol symb
  ) =>
  SMat p ('Ess @'TypeLevel symb) EssenceVL where
  sMat p _ = pure $ Ess $ symbolVal (Proxy @symb)

instance
  SMat p (Essences '[]) [EssenceVL] where
  sMat p _ = pure []

instance
  ( SMat p ess EssenceVL
  , SMat p (Essences essPath) [EssenceVL]
  ) =>
  SMat p (Essences (ess ': essPath))
         [EssenceVL] where
  sMat p _ = do
    ess     <- sMat p $ Proxy @ess
    essPath <- sMat p $ Proxy @(Essences essPath)
    pure $ ess : essPath
