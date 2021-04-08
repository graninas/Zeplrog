{-# LANGUAGE DuplicateRecordFields #-}

module ZP.TestData.KnowledgeBase.KnowledgeBase where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic
import ZP.AI.Types
import ZP.AI.StaticKnowledge
import ZP.TestData.KnowledgeBase.Essences
import ZP.TestData.KnowledgeBase.Common
import ZP.TestData.KnowledgeBase.Agents
import ZP.TestData.KnowledgeBase.Doors

import qualified Data.Map as Map
import qualified Data.Set as Set


-- agents
initKnowledgeBase1 :: IdCounter -> STM (KnowledgeBase, CommonStaticProperties)
initKnowledgeBase1 idCounterVar = do
  essencesVar <- newTVar Map.empty
  let env = KBBuilderEnv idCounterVar essencesVar

  flip runReaderT env $ do
    csp <- mkCommonStaticProperties

    ratSProp <- ratStaticProperty csp
    statProps <- sequence
      [ guardStaticProperty ratSProp csp
      ]
    essences <- lift $ readTVar essencesVar
    pure (KnowledgeBase statProps essences, csp)


-- Doors, etc.
initKnowledgeBase2 :: IdCounter -> STM (KnowledgeBase, CommonStaticProperties)
initKnowledgeBase2 idCounterVar = do
  essencesVar <- newTVar Map.empty
  let env = KBBuilderEnv idCounterVar essencesVar

  flip runReaderT env $ do
    csp <- mkCommonStaticProperties

    statProps <- sequence
      [ doorStaticProperty csp
      ]
    essences <- lift $ readTVar essencesVar
    pure (KnowledgeBase statProps essences, csp)
