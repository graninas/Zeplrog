{-# LANGUAGE DataKinds #-}

module ZP.Tests.StatMaterializationSpec where

import ZP.Prelude

import ZP.Domain.Static.Query
import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization
import qualified ZP.Domain.Static.Description as Descr
import qualified ZP.Assets.KnowledgeBase.Essences as KB

import ZP.Testing.TestData
import ZP.System.Debug
import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map



spec :: Spec
spec = do
  describe "Static materialization tests" $ do
    it "Door materialization test" $ do
      sEnv <- makeSEnv DebugDisabled

      _ <- sMat' sEnv () $ Proxy @SpecificDoor

      statProps <- readIORef $ seStaticPropertiesRef sEnv
      statEsss  <- readIORef $ seStaticEssencesRef sEnv

      -- print $ "Stat props: " <> show (Map.keys statProps)
      -- print $ "Stat essences: " <> show (Map.keys statEsss)

      length statProps `shouldBe` 2

      let (_, abstrDoor) = fromJust $ Map.lookup (StaticPropertyId 0) statProps
      let (_, door)      = fromJust $ Map.lookup (StaticPropertyId 1) statProps

      -- Descr.printDescription abstrDoor
      -- Descr.printDescription door

      case abstrDoor of
        PropDict group props scripts -> do
          let (ess, sId) = getComboId group
          sId `shouldBe` StaticPropertyId 0
          length scripts `shouldBe` 1
          length props `shouldBe` 5
          ess `shouldBe` Ess "object:abstract door"
          Map.member ess statEsss `shouldBe` True
        _ -> error "invalid materialization result"

      case door of
        PropDict group props scripts -> do
          let (ess, sId) = getComboId group
          sId `shouldBe` StaticPropertyId 1
          length scripts `shouldBe` 1
          length props `shouldBe` 5
          ess `shouldBe` Ess "object:specific door"
          Map.member ess statEsss `shouldBe` True
        _ -> error "invalid materialization result"

    it "Game materialization test 1" $ do
      sEnv <- makeSEnv DebugDisabled

      game <- sMat' sEnv () $ Proxy @(Zeplrog World1)
      let GameEnvironment _ _ _ props objs = game

      statProps <- readIORef $ seStaticPropertiesRef sEnv
      statEsss  <- readIORef $ seStaticEssencesRef sEnv

      -- print $ "Stat props: " <> show (Map.keys statProps)
      -- print $ "Stat essences: " <> show (Map.keys statEsss)

      -- print $ map (getComboId . snd) $ Map.elems statProps

      -- let (_, p1) = fromJust $ Map.lookup (StaticPropertyId 3) statProps
      -- let (_, p2) = fromJust $ Map.lookup (StaticPropertyId 9) statProps

      -- Descr.printDescription p1
      -- Descr.printDescription p2

      length statProps `shouldBe` 6
      length props `shouldBe` 3
      length objs `shouldBe` 2
