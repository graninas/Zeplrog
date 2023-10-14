{-# LANGUAGE DataKinds #-}

module ZP.Tests.StatMaterializationSpec where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Static.Model
import ZP.Domain.Static.Query
import ZP.Domain.Materializer
import qualified ZP.Assets.KnowledgeBase as KB

import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map



spec :: Spec
spec = do
  describe "Static materialization tests" $ do
    it "Door materialization test" $ do
      sEnv <- makeSEnv DebugDisabled

      door <- sMat' sEnv () $ Proxy @KB.SpecificDoor

      statProps <- readTVarIO $ seStaticPropertiesVar sEnv
      statEsss  <- readTVarIO $ seStaticEssencesVar sEnv

      -- print $ "Stat props: " <> show (Map.keys statProps)
      -- print $ "Stat essences: " <> show (Map.keys statEsss)

      case door of
        PropDict root props -> do
          let Ess ess = getEssence root
          length props `shouldBe` 6
          length statProps `shouldBe` 12
          ess `shouldBe` "object:specific door"
          Map.member (Ess ess) statEsss `shouldBe` True
        _ -> error "invalid materialization result"

    it "Game materialization test 1" $ do
      sEnv <- makeSEnv DebugDisabled

      game <- sMat' sEnv () $ Proxy @(KB.Zeplrog KB.World1)
      let GameEnvironment _ _ _ props objs = game

      statProps <- readTVarIO $ seStaticPropertiesVar sEnv
      statEsss  <- readTVarIO $ seStaticEssencesVar sEnv

      -- print $ "Stat props: " <> show (Map.keys statProps)
      -- print $ "Stat essences: " <> show (Map.keys statEsss)

      length statProps `shouldBe` 15
      length props `shouldBe` 3
      length objs `shouldBe` 2

    it "Game materialization test 2" $ do
      sEnv <- makeSEnv DebugDisabled

      game <- sMat' sEnv () $ Proxy @(KB.Zeplrog KB.World1)
      let GameEnvironment _ _ _ props objs = game

      statProps <- readTVarIO $ seStaticPropertiesVar sEnv
      statEsss  <- readTVarIO $ seStaticEssencesVar sEnv

      -- print $ "Stat props: " <> show (Map.keys statProps)
      -- print $ "Stat essences: " <> show (Map.keys statEsss)

      length statProps `shouldBe` 15
      length props `shouldBe` 3
      length objs `shouldBe` 2
