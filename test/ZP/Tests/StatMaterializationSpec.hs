{-# LANGUAGE DataKinds #-}

module ZP.Tests.StatMaterializationSpec where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Static.Model
import ZP.Domain.Static.Query
import ZP.Domain.Materializer
import qualified ZP.Assets.KnowledgeBase as KB

import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map



spec :: Spec
spec = do
  describe "Static materialization tests" $ do
    it "Door materialization test" $ do
      sEnv <- makeSEnv DebugEnabled

      door <- sMat' sEnv () $ Proxy @KB.SpecificDoor

      statProps <- readTVarIO $ seStaticPropertiesVar sEnv
      statEsss  <- readTVarIO $ seStaticEssencesVar sEnv

      print $ "Stat props: " <> show (Map.keys statProps)

      case door of
        PropDict root props -> do
          let ess = getEssence root
          length props `shouldBe` 6
          length statProps `shouldBe` 8
          show ess `shouldBe` "object:door"
          Map.member ess statEsss `shouldBe` True
        _ -> error "invalid materialization result"

    -- it "Game materialization test 1" $ do
    --   sEnv@(SEnv _ _ statPropsVar _) <- makeSEnv DebugEnabled

    --   game <- sMat' sEnv () $ Proxy @(KB.Zeplrog KB.World1)
    --   let GameEnvironment _ _ _ props objs = game

    --   statProps <- readTVarIO statPropsVar

    --   print $ "Stat props: " <> show (Map.keys statProps)

    --   length statProps `shouldBe` 10
    --   length props `shouldBe` 3
    --   length objs `shouldBe` 455

    -- it "Game materialization test 2" $ do
    --   sEnv@(SEnv _ _ statPropsVar _) <- makeSEnv DebugEnabled

    --   game <- sMat' sEnv () $ Proxy @(KB.Zeplrog' KB.World1)
    --   let GameEnvironment _ _ _ props objs = game

    --   statProps <- readTVarIO statPropsVar

    --   print $ "Stat props: " <> show (Map.keys statProps)

    --   length statProps `shouldBe` 8
    --   length props `shouldBe` 1
    --   length objs `shouldBe` 4555
