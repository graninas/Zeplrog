{-# LANGUAGE DataKinds #-}

module ZP.GameSpec where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Materialization
import qualified ZP.Assets.KnowledgeBase as KB

import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map


doorMat :: SMat.Materializer
  ( SMod.Essence 'SMod.ValueLevel
  , SMod.Property 'SMod.ValueLevel
  )
doorMat = SMat.mat $ Proxy @KB.Door

gameMat :: SMat.Materializer
  ( SMod.Game 'SMod.ValueLevel
  )
gameMat = SMat.mat $ Proxy @KB.Zeplrog

spec :: Spec
spec = do
  describe "Game tests" $ do

    it "Effect triggering test" $ do
      1 `shouldBe` 3
