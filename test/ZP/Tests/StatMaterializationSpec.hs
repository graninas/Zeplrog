{-# LANGUAGE DataKinds #-}

module ZP.Tests.StatMaterializationSpec where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Static.Model
import ZP.Domain.Static.Query
import ZP.Domain.Materializer
import qualified ZP.Assets.KnowledgeBase as KB

import ZP.TestData.World1
import ZP.Testing.Utils

import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map



spec :: Spec
spec = do
  describe "Static materialization tests" $ do
    it "Door materialization test" $ do
      sEnv <- makeSEnv DebugDisabled

      door <- sMat' sEnv () $ Proxy @SpecificDoor

      statProps <- readTVarIO $ seStaticPropertiesVar sEnv
      statEsss  <- readTVarIO $ seStaticEssencesVar sEnv

      -- print $ "Stat props: " <> show (Map.keys statProps)
      -- print $ "Stat essences: " <> show (Map.keys statEsss)

      case door of
        PropDict group props -> do
          let (ess, sId) = getComboPropertyId group
          length props `shouldBe` 3
          length statProps `shouldBe` 9
          ess `shouldBe` Ess "object:specific door"
          Map.member ess statEsss `shouldBe` True
        _ -> error "invalid materialization result"

    it "Game materialization test 1" $ do
      sEnv <- makeSEnv DebugEnabled

      game <- sMat' sEnv () $ Proxy @TestGame1
      let GameEnvironment _ _ _ props objs = game

      statProps <- readTVarIO $ seStaticPropertiesVar sEnv
      statEsss  <- readTVarIO $ seStaticEssencesVar sEnv

      -- print $ "Stat props: " <> show (Map.keys statProps)
      -- print $ "Stat essences: " <> show (Map.keys statEsss)

      length statProps `shouldBe` 20
      length props `shouldBe` 3
      length objs `shouldBe` 2
