{-# LANGUAGE DataKinds #-}

module ZP.Tests.DynMaterializationSpec where

import ZP.Prelude

import qualified ZP.Assets.KnowledgeBase as KB
import ZP.Domain.Dynamic.Model
import ZP.Domain.Static.Materialization
import ZP.Domain.Dynamic.Instantiation

import ZP.System.Debug
import ZP.Testing.Utils

import Test.Hspec
import Data.Proxy
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  describe "Dyn materialization tests" $ do

    it "Full materialization: door" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      door <- fullInst dEnv () $ Proxy @KB.SpecificDoor

      props <- readIORef $ dePropertiesRef dEnv
      -- print $ "All props: " <> show (Map.keys props)

      Map.size props `shouldBe` 8

    it "Full materialization: game" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      game <- fullInst dEnv () $ Proxy @(KB.Zeplrog KB.World1)

      props <- readIORef $ dePropertiesRef dEnv

      -- print $ "Props: " <> show (Map.keys props)

      length props `shouldBe` 208
