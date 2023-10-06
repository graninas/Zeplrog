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
  StaticPropRef
    -- N.B. This is a very strange way to browse static properties.
    -- It will require A LOT boilerplate.
    -- It's a proof that static types can be accessed from
    -- the runtime.
    :: Browser.Browse Browser.GetEssence p Essence
    => Proxy (p :: SMod.Property)
    -> StaticPropertyRef

data DynamicProperty
  = DynamicProperty
  { dpEssence       :: Essence              -- TODO: take Essence from static prop
  , dpStaticPropRef :: StaticPropertyRef
  , dpsMap          :: TVar (Map.Map Essence DynamicPropertyOwning)
  , dpValue         :: TVar (Maybe DynamicValue)
  }
