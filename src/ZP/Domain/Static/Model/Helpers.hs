{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module ZP.Domain.Static.Model.Helpers where

import ZP.Prelude
import GHC.TypeLits
import qualified Text.Show as T

import ZP.Domain.Static.Model.Common
import ZP.Domain.Static.Model.Property


-- Helpers for a more convenient type-level programming of properties.

-- | Adds a property. Removes the need to specify Essence several times.
-- TODO: complete all cases.

-- type family AddPropKV a :: PropertyKeyValueTL where
--   AddPropKV ('OwnProp prop)    = AddPropKV' prop ('OwnProp prop)
--   AddPropKV ('SharedProp prop) = AddPropKV' prop ('SharedProp prop)

-- type family AddPropKV' a self :: PropertyKeyValueTL where
--   AddPropKV' ('StaticProp group) self = AddPropKV'' group self
--   AddPropKV' ('PropVal group valDef) self = AddPropKV'' group self

-- type family AddPropKV'' a self :: PropertyKeyValueTL where
--   AddPropKV'' ('Group ess) self = PropKeyVal ess self


