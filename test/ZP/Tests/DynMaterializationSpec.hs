{-# LANGUAGE DataKinds #-}

module ZP.Tests.DynMaterializationSpec where

import ZP.Prelude

import ZP.Domain.Dynamic.Model
import ZP.Domain.Static.Materialization
import ZP.Domain.Dynamic.Instantiation
import qualified ZP.Domain.Dynamic.Description as Descr

import qualified ZP.Assets.KnowledgeBase.Essences as KB
import ZP.System.Debug
import ZP.Testing.TestData

import Test.Hspec
import Data.Proxy
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  describe "Dyn materialization tests" $ do

    it "Full materialization: door" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      door <- fullInst dEnv () $ Proxy @SpecificDoor

      -- Descr.printDescription door

      props <- readIORef $ dePropertiesRef dEnv
      -- print $ "All props: " <> show (Map.keys props)

      Map.size props `shouldBe` 1

    it "Full materialization: game" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      game <- fullInst dEnv () $ Proxy @(Zeplrog World1)

      props <- readIORef $ dePropertiesRef dEnv

      statProps <- readIORef $ seStaticPropertiesRef sEnv
      statEsss  <- readIORef $ seStaticEssencesRef sEnv

      -- dynEsss   <- readIORef $ deEssencesRef dEnv
      -- print $ "Stat props: " <> show (Map.keys statProps)
      -- print $ "Stat essences: " <> show (Map.keys statEsss)
      -- print $ "Props: " <> show (Map.keys props)

      -- let genericDoorEss = mkE @KB.EGenericDoor
      -- let genericDoorProps = map snd
      --           $ fromJust
      --           $ Map.lookup genericDoorEss dynEsss
      -- mapM_ Descr.printDescription genericDoorProps

      -- let wallEss = mkE @KB.EWall
      -- let wallProps = map snd
      --           $ fromJust
      --           $ Map.lookup wallEss dynEsss
      -- mapM_ Descr.printDescription wallProps

      length props `shouldBe` 67
