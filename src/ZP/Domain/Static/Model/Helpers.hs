{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module ZP.Domain.Static.Model.Helpers where

import ZP.Prelude
import GHC.TypeLits
import qualified Text.Show as T

import ZP.Domain.Static.Model.Common
import ZP.Domain.Static.Model.Property


type family AddPropKV a :: (PropertyKeyValue 'TypeLevel) where
  AddPropKV ('OwnProp prop)    = AddPropKV' prop ('OwnProp prop)
  AddPropKV ('SharedProp prop) = AddPropKV' prop ('SharedProp prop)

type family AddPropKV' a self :: (PropertyKeyValue 'TypeLevel) where
  AddPropKV' ('StaticProp root) self = AddPropKV'' root self
  AddPropKV' ('PropVal root valDef) self = AddPropKV'' root self

type family AddPropKV'' a self :: (PropertyKeyValue 'TypeLevel) where
  AddPropKV'' ('EssStaticRoot ess) self = PropKeyVal ess self



