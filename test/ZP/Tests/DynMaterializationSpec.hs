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
      (sEnv, dEnv@(DEnv _ sharedPropsVar)) <- makeEnvs DebugEnabled

      (essStat, doorStat) <- sMat' sEnv () $ Proxy @KB.Door
      ess  <- dMat' dEnv () essStat
      (_, door) <- dMat' dEnv () doorStat

      sharedProps <- readTVarIO sharedPropsVar
      print $ "Shared props: " <> show (Map.keys sharedProps)

      ess `shouldBe` "object:door"
      Map.size sharedProps `shouldBe` 1

    it "Full materialization: game (no macro)" $ do
      (sEnv, dEnv) <- makeEnvs DebugEnabled

      Game world cells props triggs <- fullMat dEnv ()
        $ Proxy @(KB.Zeplrog KB.World1)

      let World worldVec worldMap = world

      print $ "Props: " <> show (Map.keys props)

      length props `shouldBe` 3
      length triggs `shouldBe` 4
      length worldMap `shouldBe` 65

      case Map.lookup (mkE @KB.EDoor) props of
        Nothing -> error "Prop not found"
        Just (Property ess parentProp scriptVar propBagsVar valVar) -> do
          ess `shouldBe` "object:door"

          propBags <- readTVarIO propBagsVar

          case Map.lookup (mkE @KB.EPos) propBags of
            Just (SingleProperty (SharedProperty (DynamicPropertyRef posPropEss))) ->
              posPropEss `shouldBe` "intrinsics:pos"
            Just _ -> error "pos prop is different"
            Nothing -> error "pos prop not found"


    it "Full materialization: game (with macro)" $ do
      (sEnv@(SEnv _ statPropsVar _), dEnv) <- makeEnvs DebugEnabled

      Game world cells props triggs <- fullMat dEnv ()
        $ Proxy @(KB.Zeplrog' KB.World1)

      let World worldVec worldMap = world

      statProps <- readTVarIO statPropsVar
      print $ "Stat props: " <> show (Map.keys statProps)

      length statProps `shouldBe` 8
      length props `shouldBe` 1
      length triggs `shouldBe` 4
      length worldMap `shouldBe` 65

      case Map.lookup (mkE @KB.EDoor) props of
        Nothing -> error "Prop not found"
        Just (Property ess parentProp scriptVar propBagsVar valVar) -> do
          ess `shouldBe` "object:door"

          propBags <- readTVarIO propBagsVar

          case Map.lookup (mkE @KB.EPos) propBags of
            Just (SingleProperty (SharedProperty (DynamicPropertyRef posPropEss))) ->
              posPropEss `shouldBe` "intrinsics:pos"
            Just _ -> error "pos prop is different"
            Nothing -> error "pos prop not found"

