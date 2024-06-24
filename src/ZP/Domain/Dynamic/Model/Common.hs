{-# LANGUAGE DataKinds #-}

module ZP.Domain.Dynamic.Model.Common where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod


type Essence = String
type EssencePath = [Essence]

type TypeTag = String
type StringifiedValue = String

data Value
  = PairValue Value Value
  | IntValue Int
  | BoolValue Bool
  | StringValue String
  | TagValue SMod.TagPropertyVL Value
  | PathValue [Essence]
  | StaticPropertyRefValue SMod.StaticPropertyId
  deriving (Show, Eq, Ord)
