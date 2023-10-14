{-# LANGUAGE DataKinds #-}

module ZP.Tests.AgentsSpec where

import ZP.Prelude

import ZP.System.Debug
import qualified ZP.Domain.Static.Model as SMod
import ZP.Domain.Dynamic.Model
import ZP.Domain.Materializer
import qualified ZP.Assets.KnowledgeBase as KB

import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map


spec :: Spec
spec = do
  describe "Agents tests" $ do

    xit "Materialize actions test" $ do
      1 `shouldBe` 2
      -- (sEnv, dEnv) <- makeEnvs DebugDisabled

      -- (essStat, guardStat) <- sMat' sEnv () $ Proxy @KB.GuardActor
      -- ess <- dMat' dEnv () essStat
      -- (_, guard) <- dMat' dEnv () guardStat

      -- let Property ess parent scriptVar propsBagVar valVar = guard

      -- ess `shouldBe` "object:guard"

