{-# LANGUAGE DataKinds #-}

module ZP.StatMaterializationSpec where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization
import qualified ZP.Assets.KnowledgeBase as KB

import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map


type HPValOwnProp     = OwnProp (KB.HPVal 100)
type PosValSharedProp = SharedProp (KB.PosConst 3 5)


matDoor :: Materializer (Essence 'ValueLevel, Property 'ValueLevel)
matDoor = mat $ Proxy @KB.Door

matGame :: Materializer (GameEnvironment 'ValueLevel)
matGame = mat $ Proxy @KB.Zeplrog

spec :: Spec
spec = do
  describe "Static materialization tests" $ do
    it "Door materialization test" $ do
      (Env _ statPropsVar, (ess, door)) <- runMaterializer DebugDisabled matDoor
      statProps <- readIORef statPropsVar

      length statProps `shouldBe` 7

      case door of
        PropDict root props -> do
          length props `shouldBe` 5
        _ -> error "invalid materialization result"

    it "Game materialization test" $ do
      (Env _ statPropsVar, (ess, gameEnv)) <- runMaterializer DebugEnabled matGame
      statProps <- readIORef statPropsVar

      length statProps `shouldBe` 7

      let GameEnvironment props triggs = gameEnv

      length props `shouldBe` 543
      length triggs `shouldBe` 66
