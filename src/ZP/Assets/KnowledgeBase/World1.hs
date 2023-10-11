{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.World1 where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Hardcode.KnowledgeBase
import ZP.Assets.KnowledgeBase.Essences
import ZP.Assets.KnowledgeBase.Common
import ZP.Assets.KnowledgeBase.Doors
import ZP.Assets.KnowledgeBase.Effects

import GHC.TypeLits


type World1 = WorldData @TypeLevel
  '[ "#############"
   , "#...........#"
   , "########.####"
   , "#...........#"
   , "#############"
   ]


