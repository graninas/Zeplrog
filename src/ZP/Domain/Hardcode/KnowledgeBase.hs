{-# LANGUAGE DataKinds #-}

module ZP.Domain.Hardcode.KnowledgeBase where

import ZP.Prelude
import GHC.TypeLits
import qualified Text.Show as T

import ZP.Domain.Static.Model


-- TODO: move to the appropriate place
type EIcon = Ess @TypeLevel "system:icon"

type EGenericPos = Ess @TypeLevel "intrinsics:generic pos"
type EPos        = Ess @TypeLevel "intrinsics:pos"

-- | Generic grouping prop for world position value.
type GenericPos = StaticProp (Group EGenericPos)

-- | World position value.
type PosVal x y = PropVal
  (GroupRoot EPos GenericPos)
  (PairValue (IntValue x) (IntValue y))

-- | Derived world position value.
type DerivedPosVal = PropVal
  (GroupRoot EPos GenericPos)
  DerivedWorldPos

-- | Icon value (tmp mechanism).
type IconVal icon = PropVal (Group EIcon) (StringValue icon)
