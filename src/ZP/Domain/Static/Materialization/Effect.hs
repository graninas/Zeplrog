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
  ( Mat eff1 (Effect 'ValueLevel)
  , Mat eff2 (Effect 'ValueLevel)
  ) =>
  Mat ('EffTrigger @TypeLevel eff1 eff2) (Trigger 'ValueLevel) where
  mat _ = do
    eff1 <- mat $ Proxy @eff1
    eff2 <- mat $ Proxy @eff2
    pure $ EffTrigger eff1 eff2

instance
  ( Mat eff (Effect 'ValueLevel)
  , Mat ess (Essence 'ValueLevel)
  ) =>
  Mat ('AbilityTrigger @TypeLevel eff ess) (Trigger 'ValueLevel) where
  mat _ = do
    eff <- mat $ Proxy @eff
    ess <- mat $ Proxy @ess
    pure $ AbilityTrigger eff ess

-- Statically materialize effect

instance
  ( Mat ess (Essence 'ValueLevel)
  ) =>
  Mat ('Eff @TypeLevel ess) (Effect 'ValueLevel) where
  mat _ = do
    ess  <- mat $ Proxy @ess
    pure $ Eff ess
