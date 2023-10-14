{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Effect where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization.Materializer
import ZP.Domain.Static.Materialization.Common

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


-- Statically materialize trigger

instance
  ( SMat () eff1 EffectVL
  , SMat () eff2 EffectVL
  ) =>
  SMat () ('EffTrigger @TypeLevel eff1 eff2) TriggerVL where
  sMat () _ = do
    eff1 <- sMat () $ Proxy @eff1
    eff2 <- sMat () $ Proxy @eff2
    pure $ EffTrigger eff1 eff2

instance
  ( SMat () eff EffectVL
  , SMat () ess EssenceVL
  ) =>
  SMat () ('AbilityTrigger @TypeLevel eff ess) TriggerVL where
  sMat () _ = do
    eff <- sMat () $ Proxy @eff
    ess <- sMat () $ Proxy @ess
    pure $ AbilityTrigger eff ess

-- Statically materialize effect

instance
  ( SMat () ess EssenceVL
  ) =>
  SMat () ('Eff @TypeLevel ess) EffectVL where
  sMat () _ = do
    ess  <- sMat () $ Proxy @ess
    pure $ Eff ess
