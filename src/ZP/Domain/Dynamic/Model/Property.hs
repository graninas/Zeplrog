{-# LANGUAGE DataKinds #-}

module ZP.Domain.Dynamic.Model.Property where

import ZP.Prelude

import qualified Data.Map.Strict as Map

import qualified ZP.Domain.Static.Model as SMod
import ZP.Domain.Dynamic.Model.Common

-- | Dynamic property Id
newtype PropertyId = PropertyId Int
  deriving (Show, Eq, Ord)

-- | Property owning
data PropertyOwning
  = OwnProperty Property
  -- ^ Aggregates child props (lifetime of children
  --   doesn't exceed parent prop)
  | SharedProperty PropertyRef        -- TODO: should be dynamic prop only
  -- ^ referes to a independent prop

-- | Reference to another independent property
data PropertyRef
  = DynamicPropertyRef PropertyId
  | StaticPropertyRef SMod.StaticPropertyId

-- | Bag of properties or a single property
data PropertyBag
  = SingleProperty PropertyOwning
  | PropertyDict (TVar (Map.Map Category PropertyOwning))

-- | Dynamic property
data Property
  = Property
    { pPropertyId       :: PropertyId
    , pOwner            :: Maybe PropertyId
      -- ^ Independent property that owns this prop exclusively
    , pStaticPropertyId :: SMod.StaticPropertyId
    , pScriptVar        :: TVar (Maybe Script)
    , pPropertyBagsVar  :: TVar (Map.Map Category PropertyBag)
    }
  | ValueProperty
    { pPropertyId       :: PropertyId
    , pStaticPropertyId :: SMod.StaticPropertyId
    , pValue            :: TVar Value
    }
  | RefProperty
    { pPropertyId  :: PropertyId
    , pPropertyRef :: PropertyRef
    }
