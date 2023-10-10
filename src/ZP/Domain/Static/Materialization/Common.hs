{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Common where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization.Materializer

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


data Essences essPath

-- Statically materialize values

instance
  ( KnownNat intVal
  ) =>
  Mat ('IntValue @'TypeLevel intVal)
      (ValDef 'ValueLevel) where
  mat _ = pure
      $ IntValue
      $ fromIntegral
      $ natVal
      $ Proxy @intVal

instance
  ( Mat val1 (ValDef 'ValueLevel)
  , Mat val2 (ValDef 'ValueLevel)
  ) =>
  Mat ('PairValue @'TypeLevel val1 val2) (ValDef 'ValueLevel) where
  mat _ = do
    val1 <- mat $ Proxy @val1
    val2 <- mat $ Proxy @val2
    pure $ PairValue val1 val2

instance
  ( Mat (Essences essPath) [Essence 'ValueLevel]
  ) =>
  Mat ('PropRefValue @'TypeLevel essPath)
      (ValDef 'ValueLevel) where
  mat _ = do
    path <- mat $ Proxy @(Essences essPath)
    pure $ PropRefValue path

-- Statically materialize Essence path

instance
  Mat (Essences '[]) [Essence 'ValueLevel] where
  mat _ = pure []

instance
  ( Mat ess (Essence 'ValueLevel)
  , Mat (Essences essPath) [Essence 'ValueLevel]
  ) =>
  Mat (Essences (ess ': essPath))
      [Essence 'ValueLevel] where
  mat _ = do
    ess     <- mat $ Proxy @ess
    essPath <- mat $ Proxy @(Essences essPath)
    pure $ ess : essPath
