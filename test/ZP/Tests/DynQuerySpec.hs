{-# LANGUAGE DataKinds #-}

module ZP.Tests.DynQuerySpec where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as S
import qualified ZP.Domain.Static.Query as S
import qualified ZP.Domain.Static.Materialization as S
import qualified ZP.Assets.KnowledgeBase.Essences as KB

import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Query
import ZP.Domain.Dynamic.Instantiation

import ZP.System.Debug
import ZP.Testing.TestData
import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map


spec :: Spec
spec = do
  describe "Dynamic query spec" $ do
    it "Query string value for prop: found" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      prop <- fullInst dEnv () $ Proxy @TestProp
      path <- fullInst dEnv () $ Proxy @(S.Essences '[KB.EIntrinsics, KB.EIcon])

      valRef <- queryValueRefUnsafe path prop
      val <- readIORef valRef

      val `shouldBe` StringValue "string" "+"

    it "Query string value for prop: not found" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      prop <- fullInst dEnv () $ Proxy @TestProp
      path <- fullInst dEnv () $ Proxy @(S.Essences '[KB.EIcon])

      mbValRef <- queryValueRef path prop
      case mbValRef of
        Nothing -> pure ()
        Just _ -> error "Unexpected value"
