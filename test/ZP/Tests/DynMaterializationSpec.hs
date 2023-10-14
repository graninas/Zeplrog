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
    xit "fff" $ 1 `shouldBe` 2

    -- it "Full materialization: door" $ do
    --   (sEnv, dEnv) <- makeEnvs DebugEnabled

    --   doorStat <- sMat' sEnv () $ Proxy @KB.SpecificDoor
    --   door     <- dMat' dEnv () doorStat

    --   props <- readTVarIO $ dePropertiesVar dEnv
    --   print $ "All props: " <> show (Map.keys props)

    --   Map.size props `shouldBe` 1

    -- it "Full materialization: game" $ do
    --   (sEnv, dEnv) <- makeEnvs DebugEnabled

    --   game <- fullMat dEnv () $ Proxy @(KB.Zeplrog KB.World1)

    --   props <- readTVarIO $ dePropertiesVar dEnv
    --   esss  <- readTVarIO $ deEssencesVar dEnv

    --   print $ "Props: " <> show (Map.keys props)

    --   length props `shouldBe` 3

    --   case Map.lookup (mkE @KB.EDoor) esss of
    --     Nothing -> error "Prop not found"
    --     Just (pId, prop) -> do

    --       propBags <- readTVarIO $ pPropertyBagsVar prop

    --       case Map.lookup (mkE @KB.EPos) propBags of
    --         Just (SingleProperty (SharedProperty (DynamicPropertyRef posPropEss))) ->
    --           posPropEss `shouldBe` "intrinsics:pos"
    --         Just _ -> error "pos prop is different"
    --         Nothing -> error "pos prop not found"
    --     Just _ -> error "Invalid property"


