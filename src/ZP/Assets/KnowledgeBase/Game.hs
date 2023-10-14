{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.Game where

import ZP.Domain.Static.Model
import ZP.Domain.Static.Macro
import ZP.Domain.Hardcode.KnowledgeBase
import ZP.Assets.KnowledgeBase.Essences
import ZP.Assets.KnowledgeBase.Common
import ZP.Assets.KnowledgeBase.Doors
import ZP.Assets.KnowledgeBase.Surroundings
import ZP.Assets.KnowledgeBase.Agents
import ZP.Assets.KnowledgeBase.Effects

import Prelude (Bool(..))
import GHC.TypeLits


-- type Zeplrog' world = MGame
--   '[ UseWorld world Surroundings
--    , UseTriggers Triggers
--    , PlaceObj 2 8 GenericDoor
--    ]

type Zeplrog' world = Zeplrog world     -- tmp, no macro

type Zeplrog world = GameEnvironment
  world
  PathToIcon

  '[ SpecificDoor
   , Wall
   , EmptySpace
   ]

  '[ Obj 2 8 GenericDoor
   , Obj 3 9 Wall
   ]



-- Mad thought: different games with own props and effects,
--  but at some point they allow the charachters to cross
--  the borders between these two worlds!!
