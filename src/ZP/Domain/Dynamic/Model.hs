{-# LANGUAGE DataKinds #-}

module ZP.Domain.Dynamic.Model where

import ZP.Prelude

import GHC.TypeLits
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified ZP.Domain.Static.Model as SMod


type Essence = String

data Value
  = PairValue Value Value
  | IntValue Int
  -- | EssenceValue Description Essence
  -- | ListValue [PropertyValue]
  -- | ActingObjectValue ActingObject
  -- | ActivePropertyValue Description ActiveProperty
  -- | StaticPropertyValue StaticProperty
  deriving (Show, Eq, Ord)

data PropertyValue
  = VarValue (TVar Value)
  | ConstValue Value

data PropertyOwning
  = OwnProperty Property
  | SharedProperty PropertyRef

data PropertyRef
  = DynamicPropertyRef Essence
  | StaticPropertyRef (SMod.PropertyRoot 'SMod.ValueLevel)

data PropertyBag
  = SingleProperty PropertyOwning
  | PropertyDict (Map.Map Essence PropertyOwning)

data Property = Property
  { dpEssence    :: Essence
  , dpParentProp :: PropertyRef
  , dpPropsDict  :: TVar (Map.Map Essence PropertyBag)
  , dpPropValue  :: TVar (Maybe PropertyValue)
  }
