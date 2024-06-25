{-# LANGUAGE DataKinds #-}

module ZP.Tests.DynMaterializationSpec where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Dynamic.Model
import qualified ZP.Assets.KnowledgeBase as KB

import ZP.Domain.Materializer
import ZP.Testing.Utils

import Test.Hspec
import Data.Proxy
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  describe "Dyn materialization tests" $ do

    it "Full materialization: door" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      doorStat <- sMat' sEnv () $ Proxy @KB.SpecificDoor
      door     <- dInst' dEnv () doorStat

      props <- readTVarIO $ dePropertiesVar dEnv
      print $ "All props: " <> show (Map.keys props)

      Map.size props `shouldBe` 8

    it "Full materialization: game" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      game <- fullMat dEnv () $ Proxy @(KB.Zeplrog KB.World1)

      props <- readTVarIO $ dePropertiesVar dEnv
      esss  <- readTVarIO $ deEssencesVar dEnv

      print $ "Props: " <> show (Map.keys props)

      length props `shouldBe` 208
