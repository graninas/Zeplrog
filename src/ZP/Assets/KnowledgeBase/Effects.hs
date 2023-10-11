{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.Effects where

import ZP.Domain.Static.Model
import ZP.Domain.Hardcode.KnowledgeBase
import ZP.Assets.KnowledgeBase.Essences
import ZP.Assets.KnowledgeBase.Common

import Prelude (Bool(..))
import GHC.TypeLits

type PhysicalImpact = Eff EPhysicalImpact
type Push           = Eff EPush
type Shockwave      = Eff EShockwave
type Kick           = Eff EKick

type Triggers =
  '[ EffTrigger Push      PhysicalImpact
   , EffTrigger Shockwave PhysicalImpact
   , EffTrigger Kick      PhysicalImpact
   , AbilityTrigger PhysicalImpact EPushable
   ]

