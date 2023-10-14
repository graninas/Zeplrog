{-# LANGUAGE DataKinds #-}

module ZP.Domain.Hardcode.KnowledgeBase where

import ZP.Prelude
import GHC.TypeLits
import qualified Text.Show as T

import ZP.Domain.Static.Model


type EIcon = Ess @TypeLevel "system:icon"

type EGenericPos = Ess @TypeLevel "intrinsics:generic pos"
type EPos        = Ess @TypeLevel "intrinsics:pos"

-- | Generic grouping prop for world position value.
type GenericPos = StaticProp (EssRoot EGenericPos)

-- | World position value.
type PosVal x y = PropVal
  (PropRoot EPos GenericPos)
  (PairValue (IntValue x) (IntValue y))

-- | Derived world position value.
type DerivedPosVal = PropVal
  (PropRoot EPos GenericPos)
  DerivedWorldPos

-- | Icon value (tmp mechanism).
type IconVal icon = PropVal (EssRoot EIcon) (StringValue icon)
