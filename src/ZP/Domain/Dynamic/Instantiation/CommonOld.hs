{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Instantiation.CommonOld where

-- import ZP.Prelude

-- import qualified ZP.Domain.Static.Model as SMod
-- import qualified ZP.Domain.Static.Materialization as SMat
-- import ZP.Domain.Dynamic.Materialization.Materializer
-- import ZP.Domain.Dynamic.Model

-- import Data.Proxy
-- import System.Random
-- import qualified Data.Map.Strict as Map


-- -- Materialize group and essence

-- instance
--   DInst () SMod.EssenceVL Essence where
--   dInst _ () (SMod.Ess ess) = pure ess

-- instance
--   DInst () SMod.PropertyGroupVL (Essence, SMod.StaticPropertyId) where
--   dInst _ () (SMod.GroupId statEss sId) = do
--     ess <- dInst False () statEss
--     pure (ess, sId)
--   dInst _ () (SMod.GroupRootId statEss sId _) = do
--     ess <- dInst False () statEss
--     pure (ess, sId)

-- -- Materialize value

-- instance
--   DInst () SMod.ValDefVL Value where
--   dInst _ () (SMod.IntValue val)  = pure $ IntValue val
--   dInst _ () (SMod.BoolValue val) = pure $ BoolValue val
--   dInst _ () (SMod.StringValue val) = pure $ StringValue val
--   dInst _ () (SMod.PairValue val1 val2) = do
--     val1' <- dInst False () val1
--     val2' <- dInst False () val2
--     pure $ PairValue (val1', val2')
--   dInst _ () (SMod.PathValue essPath) = do
--     essPath' <- mapM (dInst False ()) essPath
--     -- TODO: should we ensure that the referenced property already exists?
--     pure $ PathValue essPath'
--   dInst _ () (SMod.RandomIntValue from to) = do
--     val <- randomRIO (from, to)
--     pure $ IntValue val
--   dInst _ () SMod.DerivedWorldPos =
--     pure $ PairValue (IntValue 0, IntValue 0)   --- ??????

