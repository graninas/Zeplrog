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


doorMat :: SMat.Materializer (
  SMod.Essence 'SMod.ValueLevel,
  SMod.Property 'SMod.ValueLevel)
doorMat = SMat.mat $ Proxy @KB.Door

spec :: Spec
spec = do
  describe "Materialization test" $ do

    it "Full materialization: door" $ do
      (SMat.Env statPropsVar, (ess1, statDoorProp)) <-
        SMat.runMaterializer doorMat

      statProps <- readIORef statPropsVar
      (Env _ sharedPropsVar, (ess2, doorProp)) <-
        runMaterializer statProps $ mat False statDoorProp
      sharedProps <- readTVarIO sharedPropsVar

      ess2 `shouldBe` "abc"
      Map.size sharedProps `shouldBe` 111


    -- it "Materialization: value prop" $ do
    --   (_, DynamicProperty _ _ propsVar valVar) <-
    --     runMaterializer staticProps matDoorCustom

    --   mbVal <- readTVarIO valVar
    --   case mbVal of
    --     Nothing -> pure ()
    --     Just _  -> error "Unexpected value"

    --   ehp <- mat' $ Proxy @KB.EHP

    --   props <- readTVarIO propsVar
    --   case Map.lookup ehp props of
    --     Nothing -> error "Unexpected nothing"
    --     Just (OwnDynamicProperty (DynamicProperty _ _ _ valVar2)) -> do
    --       mbVal <- readTVarIO valVar2
    --       case mbVal of
    --         Nothing -> error "Unexpected no value"
    --         Just (VarValue valVar3) -> do
    --           val3 <- readTVarIO valVar3
    --           val3 `shouldBe` IntValue 100
    --         Just _ -> error "Invalid val"
    --     Just _ -> error "Invalid dyn prop"

  --   it "Materialization: 1 shared prop" $ do
  --     (Env spsVar, _) <- runMaterializer matDoorCustom
  --     sps <- readTVarIO spsVar
  --     -- TODO: verify the prop itself
  --     Map.size sps `shouldBe` 1

  --   it "Full materialization" $ do
  --     (Env spsVar, _) <- runMaterializer matDoor
  --     sps <- readTVarIO spsVar
  --     -- TODO: verify the prop itself
  --     Map.size sps `shouldBe` 1

  -- describe "Static properties test" $ do
  --   it "Static prop is accessible from a dynamic prop 1" $ do
  --     (_, DynamicProperty _ statPropRef _ _) <- runMaterializer matDoorCustom
  --     let ess = Browser.browseDyn Browser.GetEssence statPropRef
  --     ess `shouldBe` "object:door"

  --   it "Static prop is accessible from a dynamic prop 2" $ do
  --     (_, DynamicProperty _ _ propsVar _) <- runMaterializer matDoorCustom
  --     props <- readTVarIO propsVar
  --     ehp <- mat' $ Proxy @KB.EHP
  --     case Map.lookup ehp props of
  --       Nothing -> error "Unexpected nothing"
  --       Just (OwnDynamicProperty (DynamicProperty _ statPropRef _ _)) -> do
  --         let ess = Browser.browseDyn Browser.GetEssence statPropRef
  --         ess `shouldBe` "intrinsics:hp"
