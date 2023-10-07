{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.Essences where

import ZP.Domain.Static.Model

import GHC.TypeLits

type EObserving     = Ess @TypeLevel "action:observing"
type EDiscovering   = Ess @TypeLevel "action:discovering"
type ESettingGoals  = Ess @TypeLevel "action:setting goals"
type EPlanning      = Ess @TypeLevel "action:planning"
type EFollowingPlan = Ess @TypeLevel "action:following plan"
type ENoAction      = Ess @TypeLevel "action:no action"
type EGoal          = Ess @TypeLevel "goal"

type EHP            = Ess @TypeLevel "intrinsics:hp"
type EPos           = Ess @TypeLevel "intrinsics:pos"
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

type EWand          = Ess @TypeLevel "object:wand"
type ERat           = Ess @TypeLevel "object:rat"
type EGuard         = Ess @TypeLevel "object:guard"
type EDoor          = Ess @TypeLevel "object:door"


