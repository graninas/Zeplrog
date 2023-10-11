{-# LANGUAGE DataKinds #-}

module ZP.StatMaterializationSpec where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization
import qualified ZP.Assets.KnowledgeBase as KB

import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map



spec :: Spec
spec = do
  describe "Static materialization tests" $ do
    it "Door materialization test" $ do
      sEnv@(SEnv _ statPropsVar) <- makeSEnv DebugDisabled

      (ess, door) <- sMat' sEnv $ Proxy @KB.Door

      statProps <- readTVarIO statPropsVar
      length statProps `shouldBe` 7

      case door of
        PropDict root props -> do
          length props `shouldBe` 5
        _ -> error "invalid materialization result"

    xit "Game materialization test" $ do
      sEnv@(SEnv _ statPropsVar) <- makeSEnv DebugDisabled

      game <- sMat' sEnv $ Proxy @KB.Zeplrog

      statProps <- readTVarIO statPropsVar

      length statProps `shouldBe` 7

      let GameEnvironment props triggs = game

      length props `shouldBe` 543
      length triggs `shouldBe` 66
