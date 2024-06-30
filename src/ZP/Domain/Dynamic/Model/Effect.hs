{-# LANGUAGE DataKinds #-}

module ZP.Domain.Dynamic.Model.Effect where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import ZP.Domain.Dynamic.Model.Common


newtype Effect = Effect DEssence

data EffectTrigger
  = EffTrigger Effect Effect
  | AbilityTrigger Effect DEssence
