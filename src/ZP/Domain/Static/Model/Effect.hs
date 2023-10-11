{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZP.Domain.Static.Model.Effect where

import ZP.Prelude
import GHC.TypeLits
import qualified Text.Show as T

import ZP.Domain.Static.Model.Common
import ZP.Domain.Static.Model.Script

------ Effects and triggers -----

data Effect (lvl :: Level) where
  Eff :: Essence lvl -> Effect lvl

data Trigger (lvl :: Level) where
  EffTrigger     :: Effect lvl -> Effect lvl  -> Trigger lvl
  AbilityTrigger :: Effect lvl -> Essence lvl -> Trigger lvl


------ Short identifiers ----------

type EffectTL = Effect 'TypeLevel
type EffectVL = Effect 'ValueLevel
type TriggerTL = Trigger 'TypeLevel
type TriggerVL = Trigger 'ValueLevel
