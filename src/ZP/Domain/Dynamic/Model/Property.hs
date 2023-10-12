{-# LANGUAGE DataKinds #-}

module ZP.Domain.Dynamic.Model.Property where

import ZP.Prelude

import qualified Data.Map.Strict as Map

import qualified ZP.Domain.Static.Model as SMod
import ZP.Domain.Dynamic.Model.Common
import ZP.Domain.Dynamic.Model.Script


data PropertyValue
  = VarValue (TVar Value)
  | ConstValue Value

data PropertyOwning
  = OwnProperty Property
  | SharedProperty PropertyRef

data PropertyRef
  = DynamicPropertyRef Essence
  | StaticPropertyRef (SMod.StaticPropertyRoot 'SMod.ValueLevel)

data PropertyBag
  = SingleProperty PropertyOwning
  | PropertyDict (Map.Map Essence PropertyOwning)

data Property = Property
  { pEssence    :: Essence
  , pParentProp :: PropertyRef
  , pScript     :: TVar (Maybe Script)
  , pPropBags   :: TVar (Map.Map Essence PropertyBag)
  , pPropValue  :: TVar (Maybe PropertyValue)
  }
