{-# LANGUAGE DataKinds #-}

module ZP.Domain.Dynamic.Model.CommonOld where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod


-- -- TODO: use Text
-- type Essence = String
-- type Category = Essence

-- data Value
--   = PairValue (Value, Value)
--   | IntValue Int
--   | BoolValue Bool
--   | StringValue String          -- TODO: use Text
--   -- | EssenceValue Description Essence
--   -- | ListValue [PropertyValue]
--   -- | ActingObjectValue ActingObject
--   -- | ActivePropertyValue Description ActiveProperty
--   | PathValue [Category]
--   | StaticPropertyRefValue SMod.StaticPropertyId
--   deriving (Show, Eq, Ord)
