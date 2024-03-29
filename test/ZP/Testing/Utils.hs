{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ZP.Testing.Utils where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Dynamic.Model
import ZP.Domain.Materializer
import ZP.Domain.Static.Materialization
import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization.Materializer as SMat

import Data.Proxy
import qualified Data.Map.Strict as Map
import GHC.TypeLits


mkE :: forall ess symb
    . KnownSymbol symb
    => (ess ~ 'SMod.Ess @SMod.TypeLevel symb)
    => Essence
mkE = symbolVal $ Proxy @symb
