{-# LANGUAGE DataKinds #-}

module ZP.GameSpec where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Dynamic.Model
import ZP.Domain.Materializer
import qualified ZP.Assets.KnowledgeBase as KB

import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map


spec :: Spec
spec = do
  describe "Game tests" $ do

    xit "Effect triggering test" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      1 `shouldBe` 2

