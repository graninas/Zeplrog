{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.Essences where

import ZP.Domain.Static.Model

import GHC.TypeLits

type EObserving     = Ess "action:observing"
type EDiscovering   = Ess "action:discovering"
type ESettingGoals  = Ess "action:setting goals"
type EPlanning      = Ess "action:planning"
type EFollowingPlan = Ess "action:following plan"
type ENoAction      = Ess "action:no action"
type EGoal          = Ess "goal"

type EHP            = Ess "intrinsics:hp"
type EPos           = Ess "intrinsics:pos"
type EInventory     = Ess "category:inventory"
type EIntrinsics    = Ess "category:intrinsics"
type EAbilities     = Ess "category:abilities"
type EStates        = Ess "category:states"

type EState         = Ess "state"
type EStateRef      = Ess "ref:state"
type EOpen          = Ess "open"
type EClose         = Ess "close"
type EStateOpen     = Ess "state:open"
type EStateClose    = Ess "state:close"

type EPushable      = Ess "ability:pushable"

type EWand          = Ess "object:wand"
type ERat           = Ess "object:rat"
type EGuard         = Ess "object:guard"
type EDoor          = Ess "object:door"


