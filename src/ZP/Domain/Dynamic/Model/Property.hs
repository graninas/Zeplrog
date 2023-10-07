{-# LANGUAGE DataKinds #-}

module ZP.Domain.Dynamic.Model.Property where

import ZP.Prelude

import GHC.TypeLits
import qualified Data.Map as Map
import qualified Data.Set as Set

import ZP.Domain.Dynamic.Model.Common
import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Browser.Language as Browser
import qualified ZP.Domain.Browser.Methods as Browser


data DynamicPropertyOwning
  = OwnDynamicProperty DynamicProperty
  | SharedDynamicProperty DynamicProperty

data StaticPropertyRef where
  StaticPropRef :: StaticPropertyRef        -- TODO

data DynamicProperty
  = DynamicProperty
  { dpEssence    :: DynEssence              -- TODO: take DynEssence from static prop
  , dpStaticProp :: StaticPropertyRef       -- TODO
  , dpsMap       :: TVar (Map.Map DynEssence DynamicPropertyOwning)
  , dpValue      :: TVar (Maybe DynamicValue)
  }
