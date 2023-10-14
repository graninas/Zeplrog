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

spec :: Spec
spec = do
  describe "Query spec" $ do
    it "Query string value for own prop" $ do
      sEnv <- makeSEnv DebugEnabled

      owning <- sMat' sEnv () $ Proxy @TestIconOwning
      path   <- sMat' sEnv () $ Proxy @(SMat.Essences '[KB.EIcon])

      let mbRes = queryStringForOwning path owning
      case mbRes of
        Nothing -> error "String value not found"
        Just _  -> pure ()

    it "Query string value for prop key val" $ do
      sEnv <- makeSEnv DebugEnabled

      kv   <- sMat' sEnv () $ Proxy @TestPropKeyVal
      path <- sMat' sEnv () $ Proxy @(SMat.Essences '[KB.EIcon])

      let mbRes = queryStringForKeyVals path [kv]
      case mbRes of
        Nothing -> error "String value not found"
        Just _  -> pure ()
