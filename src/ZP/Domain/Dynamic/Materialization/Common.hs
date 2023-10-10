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
  Mat (SMod.Essence 'SMod.ValueLevel) Essence where
  mat _ (SMod.Ess ess) = pure ess

instance
  Mat (SMod.StaticPropertyRoot 'SMod.ValueLevel) Essence where
  mat _ (SMod.EssStaticRoot ess) = mat False ess
  mat _ (SMod.PropStaticRoot ess _) = mat False ess   -- TODO: property itself - ???

-- Materialize value

instance
  Mat (SMod.ValDef 'SMod.ValueLevel) Value where
  mat _ (SMod.IntValue val) = pure $ IntValue val
  mat _ (SMod.BoolValue val) = pure $ BoolValue val
  mat _ (SMod.PairValue val1 val2) = do
    val1' <- mat False val1
    val2' <- mat False val2
    pure $ PairValue val1' val2'
  mat _ (SMod.PropRefValue essPath) = do
    essPath' <- mapM (mat False) essPath

    -- TODO: should we ensure that the referenced property already exists?

    pure $ PropRefValue essPath'
