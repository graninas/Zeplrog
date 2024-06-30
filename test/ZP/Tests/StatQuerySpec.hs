{-# LANGUAGE DataKinds #-}

module ZP.Tests.StatQuerySpec where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Query
import ZP.Domain.Static.Materialization
import qualified ZP.Assets.KnowledgeBase.Essences as KB
import qualified ZP.Domain.Static.Materialization as SMat

import ZP.System.Debug
import ZP.Testing.TestData
import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map


spec :: Spec
spec = do
  describe "QueryValue tests" $ do
    describe "for owning" $ do
      it "abs empty path string value: found" $ do
        sEnv <- makeSEnv DebugDisabled

        owning <- sMat' sEnv () $ Proxy @TestIconOwning
        path   <- sMat' sEnv () $ Proxy @(AbsPath '[])

        pathLength path `shouldBe` 0
        let mbRes = queryValue path owning
        mbRes `shouldBe` (Just $ StringValue "string" "+")

      it "abs path string value: not found" $ do
        sEnv <- makeSEnv DebugDisabled

        owning <- sMat' sEnv () $ Proxy @TestIconOwning
        path   <- sMat' sEnv () $ Proxy @(AbsPath '[KB.EIcon])

        pathLength path `shouldBe` 1
        let mbRes = queryValue path owning
        mbRes `shouldBe` Nothing

      it "rel empty path string value: found" $ do
        sEnv <- makeSEnv DebugDisabled

        owning <- sMat' sEnv () $ Proxy @TestIconOwning
        path   <- sMat' sEnv () $ Proxy @(RelPath '[])

        pathLength path `shouldBe` 0
        let mbRes = queryValue path owning
        mbRes `shouldBe` (Just $ StringValue "string" "+")

      it "rel path string value: not found" $ do
        sEnv <- makeSEnv DebugDisabled

        owning <- sMat' sEnv () $ Proxy @TestIconOwning
        path   <- sMat' sEnv () $ Proxy @(AbsPath '[KB.EIcon])

        pathLength path `shouldBe` 1
        let mbRes = queryValue path owning
        mbRes `shouldBe` Nothing

  describe "for key val" $ do

    it "abs path string value: found" $ do
      sEnv <- makeSEnv DebugDisabled

      kv   <- sMat' sEnv () $ Proxy @TestPropKeyVal
      path <- sMat' sEnv () $ Proxy @(AbsPath '[KB.EIcon])

      pathLength path `shouldBe` 1
      let mbRes = queryValue path kv
      mbRes `shouldBe` (Just $ StringValue "string" "+")

    it "abs path string value: not found" $ do
      sEnv <- makeSEnv DebugDisabled

      kv   <- sMat' sEnv () $ Proxy @TestPropKeyVal
      path <- sMat' sEnv () $ Proxy @(AbsPath '[KB.EHP])

      pathLength path `shouldBe` 1
      let mbRes = queryValue path kv
      mbRes `shouldBe` Nothing

    it "abs empty path string value: not found" $ do
      sEnv <- makeSEnv DebugDisabled

      kv   <- sMat' sEnv () $ Proxy @TestPropKeyVal
      path <- sMat' sEnv () $ Proxy @(AbsPath '[])

      pathLength path `shouldBe` 0
      let mbRes = queryValue path kv
      mbRes `shouldBe` Nothing

    it "rel empty path string value: found" $ do
      sEnv <- makeSEnv DebugDisabled

      kv   <- sMat' sEnv () $ Proxy @TestPropKeyVal
      path <- sMat' sEnv () $ Proxy @(RelPath '[])

      pathLength path `shouldBe` 0
      let mbRes = queryValue path kv
      mbRes `shouldBe` (Just $ StringValue "string" "+")

    it "rel string value: not found" $ do
      sEnv <- makeSEnv DebugDisabled

      kv   <- sMat' sEnv () $ Proxy @TestPropKeyVal
      path <- sMat' sEnv () $ Proxy @(RelPath '[KB.EHP])

      pathLength path `shouldBe` 1
      let mbRes = queryValue path kv
      mbRes `shouldBe` Nothing

  describe "for property" $ do

    it "abs path string value: found" $ do
      sEnv <- makeSEnv DebugDisabled

      prop <- sMat' sEnv () $ Proxy @TestProp
      path <- sMat' sEnv () $ Proxy @(AbsPath '[KB.EIntrinsics, KB.EIcon])

      pathLength path `shouldBe` 2
      let mbRes = queryValue path prop
      mbRes `shouldBe` (Just $ StringValue "string" "+")

    it "abs path string value: not found" $ do
      sEnv <- makeSEnv DebugDisabled

      prop <- sMat' sEnv () $ Proxy @TestProp
      path <- sMat' sEnv () $ Proxy @(AbsPath '[KB.EIcon])

      pathLength path `shouldBe` 1
      let mbRes = queryValue path prop
      mbRes `shouldBe` Nothing

    it "rel path string value: found" $ do
      sEnv <- makeSEnv DebugDisabled

      prop <- sMat' sEnv () $ Proxy @TestProp
      path <- sMat' sEnv () $ Proxy @(RelPath '[KB.EIcon])

      pathLength path `shouldBe` 1
      let mbRes = queryValue path prop
      mbRes `shouldBe` (Just $ StringValue "string" "+")

    it "rel path string value: not found" $ do
      sEnv <- makeSEnv DebugDisabled

      prop <- sMat' sEnv () $ Proxy @TestProp
      path <- sMat' sEnv () $ Proxy @(RelPath '[KB.EIntrinsics, KB.EIcon])

      pathLength path `shouldBe` 2
      let mbRes = queryValue path prop
      mbRes `shouldBe` Nothing

