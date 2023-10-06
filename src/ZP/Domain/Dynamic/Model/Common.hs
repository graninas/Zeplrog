{-# LANGUAGE DataKinds #-}

module ZP.Domain.Dynamic.Model.Common where

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

data DynamicValue
  = VarValue (TVar Value)
  | ConstValue Value
