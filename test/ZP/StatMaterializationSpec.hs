{-# LANGUAGE DataKinds #-}

module ZP.StatMaterializationSpec where

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
      sEnv@(SEnv _ statPropsVar) <- makeSEnv DebugDisabled

      (Ess ess, door) <- sMat' sEnv () $ Proxy @KB.Door

      statProps <- readTVarIO statPropsVar
      length statProps `shouldBe` 7

      case door of
        PropDict root props -> length props `shouldBe` 5
        _ -> error "invalid materialization result"

      ess `shouldBe` "object:door"

    it "Game materialization test" $ do
      sEnv@(SEnv _ statPropsVar) <- makeSEnv DebugDisabled

      game <- sMat' sEnv () $ Proxy @(KB.Zeplrog KB.World1)

      statProps <- readTVarIO statPropsVar

      length statProps `shouldBe` 7

      let GameEnvironment world props triggs = game

      length props `shouldBe` 1
      length triggs `shouldBe` 4
