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
  ( SMat eff1 (Effect 'ValueLevel)
  , SMat eff2 (Effect 'ValueLevel)
  ) =>
  SMat ('EffTrigger @TypeLevel eff1 eff2) (Trigger 'ValueLevel) where
  sMat _ = do
    eff1 <- sMat $ Proxy @eff1
    eff2 <- sMat $ Proxy @eff2
    pure $ EffTrigger eff1 eff2

instance
  ( SMat eff (Effect 'ValueLevel)
  , SMat ess (Essence 'ValueLevel)
  ) =>
  SMat ('AbilityTrigger @TypeLevel eff ess) (Trigger 'ValueLevel) where
  sMat _ = do
    eff <- sMat $ Proxy @eff
    ess <- sMat $ Proxy @ess
    pure $ AbilityTrigger eff ess

-- Statically materialize effect

instance
  ( SMat ess (Essence 'ValueLevel)
  ) =>
  SMat ('Eff @TypeLevel ess) (Effect 'ValueLevel) where
  sMat _ = do
    ess  <- sMat $ Proxy @ess
    pure $ Eff ess
