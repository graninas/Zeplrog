{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.Game where

import ZP.Domain.Static.Model
import ZP.Domain.Static.Macro
import ZP.Assets.KnowledgeBase.Essences
import ZP.Assets.KnowledgeBase.Common
import ZP.Assets.KnowledgeBase.Doors
import ZP.Assets.KnowledgeBase.Agents
import ZP.Assets.KnowledgeBase.Effects

import Prelude (Bool(..))
import GHC.TypeLits



type Zeplrog world = MGame
  '[ UseWorld world
   , UseTriggers Triggers
   , Displace 2 8 Door
   ]



-- Mad thought: different games with own props and effects,
--  but at some point they allow the charachters to cross
--  the borders between these two worlds!!
