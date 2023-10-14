{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.Essences where

import ZP.Domain.Static.Model
import ZP.Domain.Hardcode.KnowledgeBase

import GHC.TypeLits

type EObserving     = Ess @TypeLevel "action:observing"
type EDiscovering   = Ess @TypeLevel "action:discovering"
type ESettingGoals  = Ess @TypeLevel "action:setting goals"
type EPlanning      = Ess @TypeLevel "action:planning"
type EFollowingPlan = Ess @TypeLevel "action:following plan"
type EIdling        = Ess @TypeLevel "action:idling"
type ENoAction      = Ess @TypeLevel "action:no action"

type EActionLoop    = Ess @TypeLevel "system:action loop"
type EGoal          = Ess @TypeLevel "system:goal"


type EHP            = Ess @TypeLevel "intrinsics:hp"
type EStrength      = Ess @TypeLevel "intrinsics:strength"

type EInventory     = Ess @TypeLevel "category:inventory"
type EIntrinsics    = Ess @TypeLevel "category:intrinsics"
type EAbilities     = Ess @TypeLevel "category:abilities"
type EStates        = Ess @TypeLevel "category:states"

type EState         = Ess @TypeLevel "state"
type EStateRef      = Ess @TypeLevel "ref:state"
type EOpen          = Ess @TypeLevel "open"
type EClose         = Ess @TypeLevel "close"
type EStateOpen     = Ess @TypeLevel "state:open"
type EStateClose    = Ess @TypeLevel "state:close"

type EPushable      = Ess @TypeLevel "ability:pushable"

type EPushableScript = Ess @TypeLevel "script:pushable"

type EEmptySpace    = Ess @TypeLevel "object:empty space"
type EWall          = Ess @TypeLevel "object:wall"
type EWand          = Ess @TypeLevel "object:wand"
type ERat           = Ess @TypeLevel "object:rat"
type EGuard         = Ess @TypeLevel "object:guard"
type EAbstractDoor  = Ess @TypeLevel "object:abstract door"
type ESpecificDoor  = Ess @TypeLevel "object:specific door"
type EDoor          = Ess @TypeLevel "object:door"


type EPhysicalImpact = Ess @TypeLevel "effect:physical impact"
type EPush           = Ess @TypeLevel "effect:push"
type EShockwave      = Ess @TypeLevel "effect:shockwave"
type EKick           = Ess @TypeLevel "effect:kick"
