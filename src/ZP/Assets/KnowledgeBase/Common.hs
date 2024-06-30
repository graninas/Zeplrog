{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.Common where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Assets.KnowledgeBase.Essences

import GHC.TypeLits


type PathToIconRel = IconPath ('RelPath '[ EIcon ])
type PathToPosRel  = PosPath  ('RelPath '[ EPos ])

-- | World position value.
type GenericPos    = TagProp (TagGroup EGenericPos)
type PosVal x y    = IntPairValue x y
type PosTagVal x y = TagValue GenericPos (PosVal x y)

-- | Actor's HP.
type GenericHP   = TagProp (TagGroup EGenericHP)
type HPVal hp    = IntPairValue hp hp
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
type DerivablePosTagVal = PosTagVal 0 0

type IconVal icon = StringValue icon


type AnyProp = AbstractProp (Group EAnyProp) '[] '[]
