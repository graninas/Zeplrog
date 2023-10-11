{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.World1 where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Assets.KnowledgeBase.Essences
import ZP.Assets.KnowledgeBase.Common
import ZP.Assets.KnowledgeBase.Doors
import ZP.Assets.KnowledgeBase.Effects

import GHC.TypeLits


type World1 = WorldData @TypeLevel
  '[ "┌───────────┐"
   , "|..r........|"
   , "├───────+───┤"
   , "|....g......|"
   , "└───────────┘"
   ]


