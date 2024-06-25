{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Instantiation.Effect where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Instantiation.Instantiator
import ZP.Domain.Dynamic.Instantiation.Common

import Data.Proxy
import qualified Data.Map.Strict as Map


-- Instatiation of effect

instance
  DInst () SMod.EffectVL Effect where
  dInst _ () prop@(SMod.Eff ess) = do
    ess' <- dInst False () ess
    pure $ Effect ess'

-- Instatiation of trigger

instance
  DInst () SMod.TriggerVL EffectTrigger where
  dInst _ () prop@(SMod.EffTrigger eff1 eff2) = do
    eff1' <- dInst False () eff1
    eff2' <- dInst False () eff2
    pure $ EffTrigger eff1' eff2'
  dInst _ () prop@(SMod.AbilityTrigger eff ess) = do
    eff' <- dInst False () eff
    ess' <- dInst False () ess
    pure $ AbilityTrigger eff' ess'
