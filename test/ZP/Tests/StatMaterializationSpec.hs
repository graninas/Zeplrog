{-# LANGUAGE DataKinds #-}

module ZP.Tests.StatMaterializationSpec where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Static.Model
import ZP.Domain.Materializer
import qualified ZP.Assets.KnowledgeBase as KB

import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map



spec :: Spec
spec = do
  describe "Static materialization tests" $ do
    it "Door materialization test" $ do
      sEnv@(SEnv _ _ statPropsVar _) <- makeSEnv DebugEnabled

      (Ess ess, door) <- sMat' sEnv () $ Proxy @KB.Door

      statProps <- readTVarIO statPropsVar

      print $ "Stat props: " <> show (Map.keys statProps)

      length statProps `shouldBe` 8

      case door of
        PropDict root props -> length props `shouldBe` 6
        _ -> error "invalid materialization result"

      ess `shouldBe` "object:door"

    it "Game materialization test (no macro)" $ do
      sEnv@(SEnv _ _ statPropsVar _) <- makeSEnv DebugEnabled

      game <- sMat' sEnv () $ Proxy @(KB.Zeplrog KB.World1)
      let GameEnvironment world cells props triggs = game

      statProps <- readTVarIO statPropsVar

      print $ "Stat props: " <> show (Map.keys statProps)

      length statProps `shouldBe` 10
      length props `shouldBe` 3
      length triggs `shouldBe` 4

    it "Game materialization test (with macro)" $ do
      sEnv@(SEnv _ _ statPropsVar _) <- makeSEnv DebugEnabled

      game <- sMat' sEnv () $ Proxy @(KB.Zeplrog' KB.World1)
      let GameEnvironment world cells props triggs = game

      statProps <- readTVarIO statPropsVar

      print $ "Stat props: " <> show (Map.keys statProps)

      length statProps `shouldBe` 8
      length props `shouldBe` 1
      length triggs `shouldBe` 4
