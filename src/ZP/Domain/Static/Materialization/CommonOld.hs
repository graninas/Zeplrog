{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.CommonOld where

-- import ZP.Prelude

-- import ZP.Domain.Static.Model
-- import ZP.Domain.Static.Materialization.Materializer

-- import GHC.TypeLits
-- import Data.Proxy
-- import qualified Data.Map.Strict as Map


-- -- TODO: FIXME: bare String type

-- data Essences essPath

-- -- Statically materialize variable def

-- instance
--   ( KnownSymbol str
--   ) =>
--   SMat () str String where
--   sMat () _ = pure $ symbolVal $ Proxy @str

-- instance
--   ( KnownSymbol varName
--   ) =>
--   SMat () ('IntVar @'TypeLevel varName) VarDefVL where
--   sMat () _ = do
--     let varName = symbolVal $ Proxy @varName
--     pure $ IntVar varName

-- instance
--   ( KnownSymbol varName
--   ) =>
--   SMat () ('BoolVar @'TypeLevel varName) VarDefVL where
--   sMat () _ = do
--     let varName = symbolVal $ Proxy @varName
--     pure $ BoolVar varName

-- instance
--   ( KnownSymbol varName
--   , SMat () varDef1 VarDefVL
--   , SMat () varDef2 VarDefVL
--   ) =>
--   SMat () ('PairVar @'TypeLevel varName varDef1 varDef2)
--       VarDefVL where
--   sMat () _ = do
--     let varName = symbolVal $ Proxy @varName
--     varDef1 <- sMat () $ Proxy @varDef1
--     varDef2 <- sMat () $ Proxy @varDef2
--     pure $ PairVar varName varDef1 varDef2

-- -- Statically materialize value

-- instance
--   ( KnownNat intVal
--   ) =>
--   SMat () ('IntValue @'TypeLevel intVal)
--       ValDefVL where
--   sMat () _ = pure
--       $ IntValue
--       $ fromIntegral
--       $ natVal
--       $ Proxy @intVal

-- instance
--   ( SMat () val1 ValDefVL
--   , SMat () val2 ValDefVL
--   ) =>
--   SMat () ('PairValue @'TypeLevel val1 val2) ValDefVL where
--   sMat () _ = do
--     val1 <- sMat () $ Proxy @val1
--     val2 <- sMat () $ Proxy @val2
--     pure $ PairValue val1 val2

-- instance
--   SMat () ('BoolValue @'TypeLevel 'True) ValDefVL where
--   sMat () _ = pure $ BoolValue True

-- instance
--   ( KnownSymbol str
--   ) =>
--   SMat () ('StringValue @'TypeLevel str) ValDefVL where
--   sMat () _ = pure $ StringValue $ symbolVal $ Proxy @str

-- instance
--   SMat () ('BoolValue @'TypeLevel 'False) ValDefVL where
--   sMat () _ = pure $ BoolValue False

-- instance
--   SMat () ('DerivedWorldPos @'TypeLevel) ValDefVL where
--   sMat () _ = pure DerivedWorldPos

-- instance
--   ( SMat () (Essences essPath) [EssenceVL]
--   ) =>
--   SMat () ('PathValue @'TypeLevel essPath)
--          ValDefVL where
--   sMat () _ = do
--     path <- sMat () $ Proxy @(Essences essPath)
--     pure $ PathValue path

-- -- special values

-- instance
--   ( KnownNat from
--   , KnownNat to
--   ) =>
--   SMat () ('RandomIntValue @'TypeLevel from to)
--          ValDefVL where
--   sMat () _ = pure $ RandomIntValue
--     (fromIntegral $ natVal $ Proxy @from)
--     (fromIntegral $ natVal $ Proxy @to)

-- -- Statically materialize Essence path

-- instance
--   ( KnownSymbol symb
--   ) =>
--   SMat () ('Ess @'TypeLevel symb) EssenceVL where
--   sMat () _ = pure $ Ess $ symbolVal (Proxy @symb)

-- instance
--   SMat () (Essences '[]) [EssenceVL] where
--   sMat () _ = pure []

-- instance
--   ( SMat () ess EssenceVL
--   , SMat () (Essences essPath) [EssenceVL]
--   ) =>
--   SMat () (Essences (ess ': essPath))
--          [EssenceVL] where
--   sMat () _ = do
--     ess     <- sMat () $ Proxy @ess
--     essPath <- sMat () $ Proxy @(Essences essPath)
--     pure $ ess : essPath
