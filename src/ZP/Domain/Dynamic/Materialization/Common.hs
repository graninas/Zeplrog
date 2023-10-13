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
  DMat p (SMod.Essence 'SMod.ValueLevel) Essence where
  dMat _ p (SMod.Ess ess) = pure ess

instance
  DMat p (SMod.StaticPropertyRoot 'SMod.ValueLevel) Essence where
  dMat _ p (SMod.EssStaticRoot ess)    = dMat False p ess
  dMat _ p (SMod.PropStaticRoot ess _) = dMat False p ess   -- TODO: property itself - ???

-- Materialize value

instance
  DMat p (SMod.ValDef 'SMod.ValueLevel) Value where
  dMat _ p (SMod.IntValue val)  = pure $ IntValue val
  dMat _ p (SMod.BoolValue val) = pure $ BoolValue val
  dMat _ p (SMod.StringValue val) = pure $ StringValue val
  dMat _ p (SMod.PairValue val1 val2) = do
    val1' <- dMat False p val1
    val2' <- dMat False p val2
    pure $ PairValue (val1', val2')
  dMat _ p (SMod.PathValue essPath) = do
    essPath' <- mapM (dMat False p) essPath

    -- TODO: should we ensure that the referenced property already exists?

    pure $ PathValue essPath'
