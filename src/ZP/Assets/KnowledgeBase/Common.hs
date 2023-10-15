{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.Common where

import ZP.Domain.Static.Model
import ZP.Assets.KnowledgeBase.Essences
import ZP.Domain.Hardcode.KnowledgeBase

import GHC.TypeLits


type PathToIcon = IconPath '[ EIcon ]
type PathToPos  = PosPath  '[ EPos ]

-- | HP value: current and max
type HPVal hp = PropVal
  (Group EHP)
  (PairValue (IntValue hp) (IntValue hp))

-- | Strength random val
type StrengthRandomVal from to = PropVal
  (Group EStrength)
  (RandomIntValue from to)

-- | Abstract world object.
--   Contains the default pos property.
--   Should not be directely materialized.
type AbstractObject = AbstractProp (Group EAbstractObject)
 '[ PropKeyVal EPos (OwnProp DerivedPosVal)
  ]
