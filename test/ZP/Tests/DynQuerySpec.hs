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
    it "QueryValue abs path string value for prop: found" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      prop <- fullInst dEnv () $ Proxy @TestProp
      path <- fullInst dEnv () $ Proxy @(S.AbsPath '[KB.EIntrinsics, KB.EIcon])

      valRef <- queryValueRefUnsafe path prop
      val <- readIORef valRef

      val `shouldBe` StringValue "string" "+"

    it "QueryValue abs path string value for prop: not found" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      prop <- fullInst dEnv () $ Proxy @TestProp
      path <- fullInst dEnv () $ Proxy @(S.AbsPath '[KB.EIcon])

      mbValRef <- queryValueRef path prop
      case mbValRef of
        Nothing -> pure ()
        Just _ -> error "Unexpected value"

    it "QueryValue rel path string value for prop: found" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      prop <- fullInst dEnv () $ Proxy @TestProp
      path <- fullInst dEnv () $ Proxy @(S.RelPath '[KB.EIcon])

      valRef <- queryValueRefUnsafe path prop
      val <- readIORef valRef

      val `shouldBe` StringValue "string" "+"

    it "QueryValue rel path string value for prop: not found" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      prop <- fullInst dEnv () $ Proxy @TestProp
      path <- fullInst dEnv () $ Proxy @(S.RelPath '[])

      mbValRef <- queryValueRef path prop
      case mbValRef of
        Nothing -> pure ()
        Just _ -> error "Unexpected value"

    it "QueryValue rel path string value for prop: not found 2" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      prop <- fullInst dEnv () $ Proxy @TestProp
      path <- fullInst dEnv () $ Proxy @(S.RelPath '[KB.EIntrinsics, KB.EIcon])

      mbValRef <- queryValueRef path prop
      case mbValRef of
        Nothing -> pure ()
        Just _ -> error "Unexpected value"
