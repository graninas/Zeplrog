{-# LANGUAGE DataKinds #-}

module ZP.Domain.Dynamic.Model where

import ZP.Prelude

import GHC.TypeLits
import qualified Data.Map as Map
import qualified Data.Set as Set


data DynamicProperty = DynamicProperty
  { props :: Map.Map String ()
  }
