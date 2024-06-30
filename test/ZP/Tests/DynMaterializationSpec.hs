{-# LANGUAGE DataKinds #-}

module ZP.Tests.DynMaterializationSpec where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Instantiation
import ZP.Domain.Dynamic.Interaction
import ZP.Domain.Dynamic.Query

import qualified ZP.Domain.Dynamic.Description as Descr
import ZP.Domain.EssenceUtils
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

    it "Door's script invoke" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      door <- fullInst dEnv () $ Proxy @SpecificDoor

      let scriptEss = matEss @KB.EPushable
      let doorStatePath  = matPath @('RelPath '[KB.EState])
      let openStatePath  = matPath @OpenStateRef
      let closeStatePath = matPath @CloseStateRef

      valRef <- queryValueRefUnsafe doorStatePath door
      val1 <- readIORef valRef
      val1 `shouldBe` mkPathValue closeStatePath

      invoke scriptEss door

      val2 <- readIORef valRef
      val2 `shouldBe` mkPathValue openStatePath

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
