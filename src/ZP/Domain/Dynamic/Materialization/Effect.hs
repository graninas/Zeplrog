{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Materialization.Effect where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Materialization.Materializer
import ZP.Domain.Dynamic.Materialization.Common

import Data.Proxy
import qualified Data.Map.Strict as Map


-- Materialization of effect

instance
  DMat p (SMod.Effect 'SMod.ValueLevel) Effect where
  dMat _ p prop@(SMod.Eff ess) = do
    ess' <- dMat False p ess
    pure $ Effect ess'

-- Materialization of trigger

instance
  DMat p (SMod.Trigger 'SMod.ValueLevel) EffectTrigger where
  dMat _ p prop@(SMod.EffTrigger eff1 eff2) = do
    eff1' <- dMat False p eff1
    eff2' <- dMat False p eff2
    pure $ EffTrigger eff1' eff2'
  dMat _ p prop@(SMod.AbilityTrigger eff ess) = do
    eff' <- dMat False p eff
    ess' <- dMat False p ess
    pure $ AbilityTrigger eff' ess'
