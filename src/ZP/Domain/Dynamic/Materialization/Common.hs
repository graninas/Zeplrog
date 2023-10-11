{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Materialization.Common where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Dynamic.Materialization.Materializer
import ZP.Domain.Dynamic.Model

import Data.Proxy
import qualified Data.Map.Strict as Map


-- Materialize root and essence

instance
  DMat (SMod.Essence 'SMod.ValueLevel) Essence where
  dMat _ (SMod.Ess ess) = pure ess

instance
  DMat (SMod.StaticPropertyRoot 'SMod.ValueLevel) Essence where
  dMat _ (SMod.EssStaticRoot ess)    = dMat False ess
  dMat _ (SMod.PropStaticRoot ess _) = dMat False ess   -- TODO: property itself - ???

-- Materialize value

instance
  DMat (SMod.ValDef 'SMod.ValueLevel) Value where
  dMat _ (SMod.IntValue val) = pure $ IntValue val
  dMat _ (SMod.BoolValue val) = pure $ BoolValue val
  dMat _ (SMod.PairValue val1 val2) = do
    val1' <- dMat False val1
    val2' <- dMat False val2
    pure $ PairValue (val1', val2')
  dMat _ (SMod.PropRefValue essPath) = do
    essPath' <- mapM (dMat False) essPath

    -- TODO: should we ensure that the referenced property already exists?

    pure $ PropRefValue essPath'
