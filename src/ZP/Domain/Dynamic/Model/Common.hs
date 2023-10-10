{-# LANGUAGE DataKinds #-}

module ZP.Domain.Dynamic.Model.Common where

import ZP.Prelude

type Essence = String

data Value
  = PairValue Value Value
  | IntValue Int
  | BoolValue Bool
  -- | EssenceValue Description Essence
  -- | ListValue [PropertyValue]
  -- | ActingObjectValue ActingObject
  -- | ActivePropertyValue Description ActiveProperty
  | PropRefValue [Essence]
  deriving (Show, Eq, Ord)
