{-# LANGUAGE DataKinds #-}

module ZP.Domain.Dynamic.Model.Property where

import ZP.Prelude

import qualified Data.Map.Strict as Map

import qualified ZP.Domain.Static.Model as SMod
import ZP.Domain.Dynamic.Model.Common
import ZP.Domain.Dynamic.Model.Script


newtype PropertyId = PropertyId Int

data PropertyOwning
  = OwnProperty Property
  -- ^ aggregates child props (lifetime of children
  --   doesn't exceed parent prop)
  | SharedProperty PropertyRef
  -- ^ referes to a independent prop

data PropertyRef
  = DynamicPropertyRef PropertyId
  | StaticPropertyRef SMod.StaticPropertyRootVL

data PropertyBag
  = SingleProperty PropertyOwning
  | PropertyDict (TVar (Map.Map Category PropertyOwning))

data Property
  = Property
    { pEssence      :: Essence
    , pPropertyId   :: PropertyId
    , pOwner        :: Maybe PropertyId
      -- ^ Independent property that owns this prop exclusively
    , pStaticProp   :: SMod.StaticPropertyRootVL
    , pScriptVar    :: TVar (Maybe Script)
    , pPropBagsVar  :: TVar (Map.Map Category PropertyBag)
    }
  | ValueProperty
    { pEssence      :: Essence
    , pStaticProp   :: SMod.StaticPropertyRootVL
    , pPropValue    :: TVar Value
    }
