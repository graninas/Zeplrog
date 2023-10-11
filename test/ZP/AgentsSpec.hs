{-# LANGUAGE DataKinds #-}

module ZP.AgentsSpec where

import ZP.Prelude

import ZP.System.Debug
import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Materialization
import qualified ZP.Assets.KnowledgeBase as KB

import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map


spec :: Spec
spec = do
  describe "Agents tests" $ do

    xit "Materialize actions test" $ do

      -- gameRt <- newGameRuntime DebugEnabled
      -- actor <- materialize gameRt $ Proxy @KB.GuardActor



      1 `shouldBe` 2

