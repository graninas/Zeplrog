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


type Surroundings = '[ Wall, EmptySpace ]

type Zeplrog' world = MGame
  '[ UseWorld world Surroundings
   , UseTriggers Triggers
   , PlaceObj 2 8 Door2
   ]

type Zeplrog world = GameEnvironment
  world
  '[]
  '[ Door, Wall, EmptySpace ]
  Triggers



-- Mad thought: different games with own props and effects,
--  but at some point they allow the charachters to cross
--  the borders between these two worlds!!
