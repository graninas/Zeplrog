{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Instantiation.Common where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Dynamic.Instantiation.Instantiator
import ZP.Domain.Dynamic.Model

import Data.Proxy
import System.Random
import qualified Data.Map.Strict as Map


-- Materialize group and essence

instance
  DMat () SMod.EssenceVL Essence where
  dMat _ () (SMod.Ess ess) = pure ess

instance
  DMat () SMod.PropertyGroupVL (Essence, SMod.StaticPropertyId) where
  dMat _ () (SMod.GroupId statEss sId) = do
    ess <- dMat False () statEss
    pure (ess, sId)
  dMat _ () (SMod.GroupRootId statEss sId _) = do
    ess <- dMat False () statEss
    pure (ess, sId)

-- Materialize value

instance
  DMat () SMod.ValDefVL Value where
  dMat _ () (SMod.IntValue val)  = pure $ IntValue val
  dMat _ () (SMod.BoolValue val) = pure $ BoolValue val
  dMat _ () (SMod.StringValue val) = pure $ StringValue val
  dMat _ () (SMod.PairValue val1 val2) = do
    val1' <- dMat False () val1
    val2' <- dMat False () val2
    pure $ PairValue (val1', val2')
  dMat _ () (SMod.PathValue essPath) = do
    essPath' <- mapM (dMat False ()) essPath
    -- TODO: should we ensure that the referenced property already exists?
    pure $ PathValue essPath'
  dMat _ () (SMod.RandomIntValue from to) = do
    val <- randomRIO (from, to)
    pure $ IntValue val
  dMat _ () SMod.DerivedWorldPos =
    pure $ PairValue (IntValue 0, IntValue 0)   --- ??????

