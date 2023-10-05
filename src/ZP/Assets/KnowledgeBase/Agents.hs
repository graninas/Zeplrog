{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module ZP.Assets.KnowledgeBase.Agents where

import ZP.Domain.Static.Property
import ZP.Assets.KnowledgeBase.Essences
import ZP.Assets.KnowledgeBase.Common

import GHC.TypeLits


-- Why this is not a compound object?
type FireWand = StaticProp (EssStaticRoot EWand)
type IceWand  = StaticProp (EssStaticRoot EWand)
type KillGoal = StaticProp (EssStaticRoot EGoal)


type RatActor = PropDict (EssRoot ERat)
  '[ PropKeyBag EIntrinsics '[]
   ]

type GuardActor = PropDict (EssRoot EGuard)
  '[ PropKeyBag EIntrinsics '[]
   ]




