{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.Common where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Assets.KnowledgeBase.Essences

import GHC.TypeLits


type PathToIcon = IconPath '[ EIcon ]
type PathToPos  = PosPath  '[ EPos ]

-- | World position value.
type GenericPos    = TagProp (TagGroup EGenericPos)
type PosVal x y    = PairValue (IntValue x) (IntValue y)
type PosTagVal x y = TagValue GenericPos (PosVal x y)

-- | Actor's HP.
type GenericHP   = TagProp (TagGroup EGenericHP)
type HPVal hp    = PairValue (IntValue hp) (IntValue hp)
type HPTagVal hp = TagValue GenericHP (HPVal hp)


-- -- | Strength random val
-- type StrengthRandomVal from to = PropVal
--   (Group EStrength)
--   (RandomIntValue from to)

-- -- | Derived world position value.
-- type DerivedPosVal = PropVal
--   (GroupRoot EPos GenericPos)
--   DerivedWorldPos

-- | Derived world position value.
type DerivablePosTagVal x y = OverriddableValue (PosTagVal x y)

type IconVal icon = StringValue icon


type AnyProp = AbstractProp (Group EAnyProp) '[] '[]
