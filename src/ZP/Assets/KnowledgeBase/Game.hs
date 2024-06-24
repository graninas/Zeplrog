{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.Game where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Macro
import ZP.Assets.KnowledgeBase.Essences
import ZP.Assets.KnowledgeBase.Common
import ZP.Assets.KnowledgeBase.Doors
import ZP.Assets.KnowledgeBase.Surroundings
import ZP.Assets.KnowledgeBase.Agents
import ZP.Assets.KnowledgeBase.Effects

import GHC.TypeLits


type Zeplrog world = GameEnvironment
  world
  PathToIcon
  PathToPos

  -- Static props for the instantiation from the world data
  '[ EmptySpace
   , SpecificDoor
   , Wall
   ]

  -- Objects (static props placed into the world separately)
  '[ Obj 2 8 GenericDoor
   , Obj 3 9 Wall
   ]



-- Mad thought: different games with own props and effects,
--  but at some point they allow the charachters to cross
--  the borders between these two worlds!!
