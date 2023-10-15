{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module ZP.Assets.KnowledgeBase.Agents where

import ZP.Domain.Static.Model
import ZP.Domain.Hardcode.KnowledgeBase
import ZP.Assets.KnowledgeBase.Essences
import ZP.Assets.KnowledgeBase.Common

import GHC.TypeLits


-- Why this is not a compound object?
type FireWand = StaticProp (Group EWand)
type IceWand  = StaticProp (Group EWand)
type KillGoal = StaticProp (Group EGoal)


type Observing     = StaticProp (Group EObserving)
type Discovering   = StaticProp (Group EDiscovering)
type SettingGoals  = StaticProp (Group ESettingGoals)
type Planning      = StaticProp (Group EPlanning)
type FollowingPlan = StaticProp (Group EFollowingPlan)


type ActionLoop = PropDict (Group EActionLoop)
  '[ AddPropKV (SharedProp Observing)
   , AddPropKV (SharedProp Discovering)
   , AddPropKV (SharedProp SettingGoals)
   , AddPropKV (SharedProp Planning)
   , AddPropKV (SharedProp FollowingPlan)
   ]


type RatActor = DerivedProp ERat AbstractObject
  '[ AddPropKV (OwnProp (HPVal 20))
   , AddPropKV (OwnProp DerivedPosVal)
   , AddPropKV (SharedProp ActionLoop)
   ]

type GuardActor = DerivedProp EGuard AbstractObject
  '[ AddPropKV (OwnProp (HPVal 100))
   , AddPropKV (OwnProp (StrengthRandomVal 10 20))
   , AddPropKV (OwnProp DerivedPosVal)
   , AddPropKV (SharedProp ActionLoop)
   ]



