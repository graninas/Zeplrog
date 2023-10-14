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
  DMat () SMod.EffectVL Effect where
  dMat _ () prop@(SMod.Eff ess) = do
    ess' <- dMat False () ess
    pure $ Effect ess'

-- Materialization of trigger

instance
  DMat () SMod.TriggerVL EffectTrigger where
  dMat _ () prop@(SMod.EffTrigger eff1 eff2) = do
    eff1' <- dMat False () eff1
    eff2' <- dMat False () eff2
    pure $ EffTrigger eff1' eff2'
  dMat _ () prop@(SMod.AbilityTrigger eff ess) = do
    eff' <- dMat False () eff
    ess' <- dMat False () ess
    pure $ AbilityTrigger eff' ess'
