{-# LANGUAGE DataKinds #-}

module ZP.DynMaterializationSpec where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Materialization
import qualified ZP.Assets.KnowledgeBase as KB

import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map


doorMat :: SMat.Materializer
  ( SMod.Essence 'SMod.ValueLevel
  , SMod.Property 'SMod.ValueLevel
  )
doorMat = SMat.mat $ Proxy @KB.Door

gameMat :: SMat.Materializer
  ( SMod.Game 'SMod.ValueLevel
  )
gameMat = SMat.mat $ Proxy @KB.Zeplrog


spec :: Spec
spec = do
  describe "Materialization tests" $ do

    it "Full materialization: door" $ do
      (SMat.Env _ statPropsVar, (ess1, statDoorProp)) <-
        SMat.runMaterializer SMat.DebugDisabled doorMat

      statProps <- readIORef statPropsVar
      (Env _ sharedPropsVar, (ess2, doorProp)) <-
        runMaterializer statProps $ mat False statDoorProp
      sharedProps <- readTVarIO sharedPropsVar

      ess2 `shouldBe` "object:door"
      Map.size sharedProps `shouldBe` 1

    it "Full materialization: game" $ do
      (SMat.Env _ statPropsVar, statGame) <-
        SMat.runMaterializer SMat.DebugDisabled gameMat
      statProps <- readIORef statPropsVar
      (Env _ _, game) <-
        runMaterializer statProps $ mat False statGame

      let Game props triggs = game

      length props `shouldBe` 1
      length triggs `shouldBe` 4
