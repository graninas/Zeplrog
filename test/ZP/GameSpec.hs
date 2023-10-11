{-# LANGUAGE DataKinds #-}

module ZP.GameSpec where

import ZP.Prelude

import ZP.System.Debug
import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Materialization
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

