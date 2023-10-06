{-# LANGUAGE DataKinds #-}

module ZP.Domain.Dynamic.Model where

import ZP.Prelude

import GHC.TypeLits
import qualified Data.Map as Map
import qualified Data.Set as Set


type Essence = String


data DynamicPropertyOwning
  = OwnDynamicProperty DynamicProperty
  | SharedDynamicProperty DynamicProperty

data Value
  = PairValue Value Value
  | IntValue Int
  -- | EssenceValue Description Essence
  -- | ListValue [PropertyValue]
  -- | ActingObjectValue ActingObject
  -- | ActivePropertyValue Description ActiveProperty
  -- | StaticPropertyValue StaticProperty
  deriving (Show, Eq, Ord)

data DynamicValue
  = VarValue (TVar Value)
  | ConstValue Value

data DynamicProperty
  = DynamicProperty
  { dynPropEssence :: Essence
  , dynPropsMap    :: TVar (Map.Map Essence DynamicPropertyOwning)
  , dynPropValue   :: TVar (Maybe DynamicValue)
  }
