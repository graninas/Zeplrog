{-# LANGUAGE DataKinds #-}

module ZP.Tests.QuerySpec where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Static.Model
import ZP.Domain.Static.Query
import ZP.Domain.Materializer
import qualified ZP.Domain.Static.Materialization as SMat
import qualified ZP.Assets.KnowledgeBase as KB
import qualified ZP.Domain.Hardcode.KnowledgeBase

import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map


type TestIconOwning = OwnProp (KB.IconVal "+")
type TestPropKeyVal = PropKeyVal KB.EIcon TestIconOwning

type TestProp = PropDict (Group KB.EIntrinsics)
  '[ TestPropKeyVal
   ]


type Wall = PropDict (Group KB.EWall)
  '[ PropKeyVal KB.EIcon (OwnProp (KB.IconVal "#"))
   ]

spec :: Spec
spec = do
  describe "Query spec" $ do
    it "Query string value for own prop" $ do
      sEnv <- makeSEnv DebugDisabled

      owning <- sMat' sEnv () $ Proxy @TestIconOwning
      path   <- sMat' sEnv () $ Proxy @(SMat.Essences '[KB.EIcon])

      let mbRes = queryStringValueForOwning path owning
      case mbRes of
        Nothing  -> error "String value not found"
        Just val -> val `shouldBe` "+"

    it "Query string value for prop key val" $ do
      sEnv <- makeSEnv DebugDisabled

      kv   <- sMat' sEnv () $ Proxy @TestPropKeyVal
      path <- sMat' sEnv () $ Proxy @(SMat.Essences '[KB.EIcon])

      let mbRes = queryStringValueForKeyVals path [kv]
      case mbRes of
        Nothing -> error "String value not found"
        Just val -> val `shouldBe` "+"

    it "Query string value for prop not relative" $ do
      sEnv <- makeSEnv DebugDisabled

      prop <- sMat' sEnv () $ Proxy @TestProp
      path <- sMat' sEnv () $ Proxy @(SMat.Essences '[KB.EIntrinsics, KB.EIcon])

      let mbRes = queryStringValue path prop
      case mbRes of
        Nothing -> error "String value not found"
        Just val -> val `shouldBe` "+"

    it "Query string value for prop relative" $ do
      sEnv <- makeSEnv DebugDisabled

      prop <- sMat' sEnv () $ Proxy @TestProp
      path <- sMat' sEnv () $ Proxy @(SMat.Essences '[KB.EIcon])

      let mbRes = queryStringValueRelative path prop
      case mbRes of
        Nothing -> error "String value not found"
        Just val -> val `shouldBe` "+"
