{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.Common where

import ZP.Domain.Static.Model
import ZP.Assets.KnowledgeBase.Essences
import ZP.Domain.Hardcode.KnowledgeBase

import GHC.TypeLits

-- | HP value: current and max
type HPVal hp = PropVal
  (EssRoot EHP)
  (PairValue (IntValue hp) (IntValue hp))

-- | Strength random val
type StrengthRandomVal from to = PropVal
  (EssRoot EStrength)
  (RandomIntValue from to)

-- | Icon value (tmp mechanism)
type IconVal icon = PropVal (EssRoot EIcon) (StringValue icon)


-- | Derived world position value.
type DerivedWorldPosVal = PropVal (EssRoot EPos) DerivedWorldPos
