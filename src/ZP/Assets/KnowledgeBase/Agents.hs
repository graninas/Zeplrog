{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module ZP.Assets.KnowledgeBase.Agents where

import ZP.Domain.Static.Model
import ZP.Domain.Hardcode.KnowledgeBase
import ZP.Assets.KnowledgeBase.Essences
import ZP.Assets.KnowledgeBase.Common
import ZP.Domain.Static.Model.Helpers

import GHC.TypeLits


-- Why this is not a compound object?
type FireWand = StaticProp (EssRoot EWand)
type IceWand  = StaticProp (EssRoot EWand)
type KillGoal = StaticProp (EssRoot EGoal)


type Observing     = StaticProp (EssRoot EObserving)
type Discovering   = StaticProp (EssRoot EDiscovering)
type SettingGoals  = StaticProp (EssRoot ESettingGoals)
type Planning      = StaticProp (EssRoot EPlanning)
type FollowingPlan = StaticProp (EssRoot EFollowingPlan)


type ActionLoop = PropDict (EssRoot EActionLoop)
  '[ AddPropKV (SharedProp Observing)
   , AddPropKV (SharedProp Discovering)
   , AddPropKV (SharedProp SettingGoals)
   , AddPropKV (SharedProp Planning)
   , AddPropKV (SharedProp FollowingPlan)
   ]


type RatActor = PropDict (EssRoot ERat)
  '[ AddPropKV (OwnProp (HPVal 20))
   , AddPropKV (OwnProp DerivedWorldPosVal)
   , AddPropKV (SharedProp ActionLoop)
   ]

type GuardActor = PropDict (EssRoot EGuard)
  '[ AddPropKV (OwnProp (HPVal 100))
   , AddPropKV (OwnProp (StrengthRandomVal 10 20))
   , AddPropKV (OwnProp DerivedWorldPosVal)
   , AddPropKV (SharedProp ActionLoop)
   ]



