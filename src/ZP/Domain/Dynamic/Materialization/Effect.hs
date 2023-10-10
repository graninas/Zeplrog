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
  Mat (SMod.Effect 'SMod.ValueLevel) Effect where
  mat _ prop@(SMod.Eff ess) = do
    ess' <- mat False ess
    pure $ Effect ess'

-- Materialization of trigger

instance
  Mat (SMod.Trigger 'SMod.ValueLevel) EffectTrigger where
  mat _ prop@(SMod.EffTrigger eff1 eff2) = do
    eff1' <- mat False eff1
    eff2' <- mat False eff2
    pure $ EffTrigger eff1' eff2'
  mat _ prop@(SMod.AbilityTrigger eff ess) = do
    eff' <- mat False eff
    ess' <- mat False ess
    pure $ AbilityTrigger eff' ess'
