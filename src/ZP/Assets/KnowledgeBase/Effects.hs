{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.Effects where

import ZP.Domain.Static.Model
import ZP.Assets.KnowledgeBase.Essences
import ZP.Assets.KnowledgeBase.Common

import Prelude (Bool(..))
import GHC.TypeLits

type PhysicalImpact = Eff EPhysicalImpact
type Push           = Eff EPhysicalImpact
type Shockwave      = Eff EPhysicalImpact
type Kick           = Eff EPhysicalImpact

type Triggers =
  '[ EffTrigger Push      PhysicalImpact
   , EffTrigger Shockwave PhysicalImpact
   , EffTrigger Kick      PhysicalImpact
   , AbilityTrigger PhysicalImpact EPushable
   ]

