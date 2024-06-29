{-# LANGUAGE DataKinds #-}

module ZP.Tests.StatQuerySpec where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Query
import ZP.Domain.Static.Materialization
import qualified ZP.Assets.KnowledgeBase as KB
import qualified ZP.Domain.Static.Materialization as SMat

import ZP.System.Debug
import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map


type TestIconOwning = OwnVal (KB.IconVal "+")
type TestPropKeyVal = PropKeyVal KB.EIcon TestIconOwning

type TestProp = DerivedProp KB.EIntrinsics KB.AnyProp
  '[ TestPropKeyVal
   ]
  '[]


type Wall = DerivedProp KB.EWall KB.AnyProp
  '[ PropKeyVal KB.EIcon (OwnVal (KB.IconVal "#"))
   ]
  '[]

spec :: Spec
spec = do
  describe "Query spec" $ do
    it "Query string value for owning: found" $ do
      sEnv <- makeSEnv DebugDisabled

      owning <- sMat' sEnv () $ Proxy @TestIconOwning
      path   <- sMat' sEnv () $ Proxy @(SMat.Essences '[])

      length path `shouldBe` 0
      let mbRes = queryValue path owning
      mbRes `shouldBe` (Just $ StringValue "+")

    it "Query string value for owning: not found" $ do
      sEnv <- makeSEnv DebugDisabled

      owning <- sMat' sEnv () $ Proxy @TestIconOwning
      path   <- sMat' sEnv () $ Proxy @(SMat.Essences '[KB.EIcon])

      length path `shouldBe` 1
      let mbRes = queryValue path owning
      mbRes `shouldBe` Nothing

    it "Query string value for prop key val: found" $ do
      sEnv <- makeSEnv DebugDisabled

      kv   <- sMat' sEnv () $ Proxy @TestPropKeyVal
      path <- sMat' sEnv () $ Proxy @(SMat.Essences '[KB.EIcon])

      length path `shouldBe` 1
      let mbRes = queryValue path kv
      mbRes `shouldBe` (Just $ StringValue "+")

    it "Query string value for prop key val: not found" $ do
      sEnv <- makeSEnv DebugDisabled

      kv   <- sMat' sEnv () $ Proxy @TestPropKeyVal
      path <- sMat' sEnv () $ Proxy @(SMat.Essences '[KB.EHP])

      length path `shouldBe` 1
      let mbRes = queryValue path kv
      mbRes `shouldBe` Nothing

    it "Query string value for prop key val: not found 2" $ do
      sEnv <- makeSEnv DebugDisabled

      kv   <- sMat' sEnv () $ Proxy @TestPropKeyVal
      path <- sMat' sEnv () $ Proxy @(SMat.Essences '[])

      length path `shouldBe` 0
      let mbRes = queryValue path kv
      mbRes `shouldBe` Nothing

    it "Query string value for prop: found" $ do
      sEnv <- makeSEnv DebugDisabled

      prop <- sMat' sEnv () $ Proxy @TestProp
      path <- sMat' sEnv () $ Proxy @(SMat.Essences '[KB.EIntrinsics, KB.EIcon])

      length path `shouldBe` 2
      let mbRes = queryValue path prop
      mbRes `shouldBe` (Just $ StringValue "+")

    it "Query string value for prop: not found" $ do
      sEnv <- makeSEnv DebugDisabled

      prop <- sMat' sEnv () $ Proxy @TestProp
      path <- sMat' sEnv () $ Proxy @(SMat.Essences '[KB.EIcon])

      length path `shouldBe` 1
      let mbRes = queryValue path prop
      mbRes `shouldBe` Nothing
