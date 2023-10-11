{-# LANGUAGE DataKinds #-}

module ZP.DynMaterializationSpec where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Dynamic.Model
import qualified ZP.Assets.KnowledgeBase as KB

import ZP.Domain.Materializer

import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  describe "Dyn materialization tests" $ do

    it "Full materialization: door" $ do
      (sEnv, dEnv@(DEnv _ sharedPropsVar)) <- makeEnvs DebugDisabled

      (essStat, doorStat) <- sMat' sEnv $ Proxy @KB.Door
      ess  <- dMat' dEnv essStat
      (_, door) <- dMat' dEnv doorStat

      sharedProps <- readTVarIO sharedPropsVar

      ess `shouldBe` "object:door"
      Map.size sharedProps `shouldBe` 1

    it "Full materialization: game" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      Game _ props triggs <- fullMat dEnv $ Proxy @(KB.Zeplrog KB.World1)

      length props `shouldBe` 1
      length triggs `shouldBe` 4
