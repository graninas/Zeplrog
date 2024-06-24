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
  = OwnVal (IORef Value)
  -- ^ Mutable dynamic value
  | OwnDict (IORef (Map.Map Essence Property))
  -- ^ Multiple child props
  | OwnProp Property
  -- ^ Single child prop
  | SharedProp PropertyRef
  -- ^ Reference to an independent prop

-- | Reference to another independent property
data PropertyRef
  = DynamicPropRef PropertyId
  | StaticPropRef SMod.StaticPropertyId

data DynamicScript = DynScript (IO ())

-- | Dynamic property
data Property
  = TagPropRef SMod.TagPropertyVL
  | Prop
    { pPropertyId       :: PropertyId
      -- ^ Unique Id for a property instance
    , pOwner            :: Maybe PropertyId
      -- ^ Independent property that owns this prop exclusively
    , pStaticPropertyId :: SMod.StaticPropertyId
      -- ^ Source property for this one
    , pFieldsRef        :: IORef (Map.Map Essence PropertyOwning)
      -- ^ Child properties
    , pScripts          :: Map.Map Essence DynamicScript
    }
