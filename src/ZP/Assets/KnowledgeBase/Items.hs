{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module ZP.Assets.KnowledgeBase.Items where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Assets.KnowledgeBase.Essences
import ZP.Assets.KnowledgeBase.Common

import GHC.TypeLits


type FireWand = TagProp (TagGroup EWand)
type IceWand  = TagProp (TagGroup EWand)
type KillGoal = TagProp (TagGroup EGoal)

