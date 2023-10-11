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
  SMat (Rowss '[]) [String] where
  sMat _ = pure []

instance
  ( KnownSymbol row
  , SMat (Rowss rows) [String]
  ) =>
  SMat (Rowss (row ': rows)) [String] where
  sMat _ = do
    let row = symbolVal $ Proxy @row
    rows <- sMat $ Proxy @(Rowss rows)
    pure $ row : rows

-- Statically materialize world

instance
  ( SMat (Rowss rows) [String]
  ) =>
  SMat ('WorldData @TypeLevel rows) (World 'ValueLevel) where
  sMat _ = do
    rows <- sMat $ Proxy @(Rowss rows)
    pure $ WorldData rows
