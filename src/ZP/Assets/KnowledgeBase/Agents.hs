{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module ZP.Assets.KnowledgeBase.Agents where

import ZP.Domain.Static.Model
import ZP.Assets.KnowledgeBase.Essences
import ZP.Assets.KnowledgeBase.Common
import ZP.Domain.Static.Model.Helpers

import GHC.TypeLits


-- Why this is not a compound object?
type FireWand = StaticProp (EssStaticRoot EWand)
type IceWand  = StaticProp (EssStaticRoot EWand)
type KillGoal = StaticProp (EssStaticRoot EGoal)


type Observing     = StaticProp (EssStaticRoot EObserving)
type Discovering   = StaticProp (EssStaticRoot EDiscovering)
type SettingGoals  = StaticProp (EssStaticRoot ESettingGoals)
type Planning      = StaticProp (EssStaticRoot EPlanning)
type FollowingPlan = StaticProp (EssStaticRoot EFollowingPlan)


type ActionLoop = PropDict (EssStaticRoot EActionLoop)
  '[ AddPropKV (SharedProp Observing)
   , AddPropKV (SharedProp Discovering)
   , AddPropKV (SharedProp SettingGoals)
   , AddPropKV (SharedProp Planning)
   , AddPropKV (SharedProp FollowingPlan)
   ]


type RatActor = PropDict (EssStaticRoot ERat)
  '[ AddPropKV (OwnProp (HPVal 20))
   , AddPropKV (SharedProp (PosConst 5 8))

   , AddPropKV (SharedProp ActionLoop)
   ]

type GuardActor = PropDict (EssStaticRoot EGuard)
  '[ AddPropKV (OwnProp (HPVal 100))
   , AddPropKV (SharedProp (PosConst 1 3))
   ]



