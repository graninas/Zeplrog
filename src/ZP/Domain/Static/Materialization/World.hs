{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.World where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization.Materializer
import ZP.Domain.Static.Materialization.Common

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


data Rowss ps

-- Statically materialize rows

-- TODO: FIXME: bare String type

instance
  SMat p (Rowss '[]) [String] where
  sMat p _ = pure []

instance
  ( KnownSymbol row
  , SMat p (Rowss rows) [String]
  ) =>
  SMat p (Rowss (row ': rows)) [String] where
  sMat p _ = do
    let row = symbolVal $ Proxy @row
    rows <- sMat p $ Proxy @(Rowss rows)
    pure $ row : rows

-- Statically materialize world

instance
  ( SMat p (Rowss rows) [String]
  ) =>
  SMat p ('WorldData @TypeLevel rows) (World 'ValueLevel) where
  sMat p _ = do
    rows <- sMat p $ Proxy @(Rowss rows)
    pure $ WorldData rows
