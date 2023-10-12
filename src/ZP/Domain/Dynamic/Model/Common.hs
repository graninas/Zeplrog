{-# LANGUAGE DataKinds #-}

module ZP.Domain.Dynamic.Model.Common where

import ZP.Prelude

-- TODO: use Text
type Essence = String

type ObjectId = Int

data Value
  = PairValue (Value, Value)
  | IntValue Int
  | BoolValue Bool
  | StringValue String          -- TODO: use Text
  -- | EssenceValue Description Essence
  -- | ListValue [PropertyValue]
  -- | ActingObjectValue ActingObject
  -- | ActivePropertyValue Description ActiveProperty
  | PropRefValue [Essence]
  deriving (Show, Eq, Ord)
