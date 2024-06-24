{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module ZP.Assets.KnowledgeBase.Agents where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Assets.KnowledgeBase.Essences
import ZP.Assets.KnowledgeBase.Common

import GHC.TypeLits

type Observing     = TagProp (TagGroup EObserving)
type Discovering   = TagProp (TagGroup EDiscovering)
type SettingGoals  = TagProp (TagGroup ESettingGoals)
type Planning      = TagProp (TagGroup EPlanning)
type FollowingPlan = TagProp (TagGroup EFollowingPlan)



-- Tmp, find a better way to encode this
type AbstractActionLoop = AbstractDerivedProp
  (Ess @TypeLevel "abstract act loop") AnyProp
  '[
   ]
  '[]

type ActionLoop = DerivedProp EActionLoop AbstractActionLoop
  '[]
  '[]
  -- '[ AddPropKV (SharedProp Observing)
  --  , AddPropKV (SharedProp Discovering)
  --  , AddPropKV (SharedProp SettingGoals)
  --  , AddPropKV (SharedProp Planning)
  --  , AddPropKV (SharedProp FollowingPlan)
  --  ]


-- type RatActor = PropDict (Group ERat)
--   '[ AddPropKV (OwnProp (HPVal 20))
--    , AddPropKV (OwnProp DerivedPosVal)
--    , AddPropKV (SharedProp ActionLoop)
--    ]

-- type GuardActor = PropDict (Group EGuard)
--   '[ AddPropKV (OwnProp (HPVal 100))
--    , AddPropKV (OwnProp (StrengthRandomVal 10 20))
--    , AddPropKV (OwnProp DerivedPosVal)
--    , AddPropKV (SharedProp ActionLoop)
--    ]



