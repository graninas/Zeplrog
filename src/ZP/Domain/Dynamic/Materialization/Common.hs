{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Materialization.Common where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Dynamic.Materialization.Materializer
import ZP.Domain.Dynamic.Model

import Data.Proxy
import System.Random
import qualified Data.Map.Strict as Map


-- Materialize root and essence

instance
  DMat p SMod.EssenceVL Essence where
  dMat _ p (SMod.Ess ess) = pure ess

instance
  DMat p SMod.PropertyRootVL Essence where
  dMat _ p (SMod.EssRoot ess)    = dMat False p ess
  dMat _ p (SMod.PropRoot ess _) = dMat False p ess   -- TODO: property itself - ???

-- Materialize value

instance
  DMat p SMod.ValDefVL Value where
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
  dMat _ p (SMod.RandomIntValue from to) = do
    val <- randomRIO (from, to)
    pure $ IntValue val

