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
  ( SMat p eff1 (Effect 'ValueLevel)
  , SMat p eff2 (Effect 'ValueLevel)
  ) =>
  SMat p ('EffTrigger @TypeLevel eff1 eff2) (Trigger 'ValueLevel) where
  sMat p _ = do
    eff1 <- sMat p $ Proxy @eff1
    eff2 <- sMat p $ Proxy @eff2
    pure $ EffTrigger eff1 eff2

instance
  ( SMat p eff (Effect 'ValueLevel)
  , SMat p ess (Essence 'ValueLevel)
  ) =>
  SMat p ('AbilityTrigger @TypeLevel eff ess) (Trigger 'ValueLevel) where
  sMat p _ = do
    eff <- sMat p $ Proxy @eff
    ess <- sMat p $ Proxy @ess
    pure $ AbilityTrigger eff ess

-- Statically materialize effect

instance
  ( SMat p ess (Essence 'ValueLevel)
  ) =>
  SMat p ('Eff @TypeLevel ess) (Effect 'ValueLevel) where
  sMat p _ = do
    ess  <- sMat p $ Proxy @ess
    pure $ Eff ess
