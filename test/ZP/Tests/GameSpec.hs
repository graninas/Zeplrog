{-# LANGUAGE DataKinds #-}


{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


module ZP.Tests.GameSpec where

import ZP.Prelude

import qualified ZP.Assets.KnowledgeBase as KB
import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Query
import ZP.Domain.Dynamic.Instantiation
import ZP.Domain.EssenceUtils
import ZP.Testing.TestData

import ZP.System.TypeSelector.Granular
import ZP.System.Debug
import Test.Hspec
import Data.Proxy
import qualified Data.Map.Strict as Map



spec :: Spec
spec = do
  describe "Game tests" $ do

    it "World items displacement test" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      game <- fullInst dEnv () $ Proxy @(Zeplrog World1)

      props <- readIORef $ dePropertiesRef dEnv

      statProps <- readIORef $ seStaticPropertiesRef sEnv
      statEsss  <- readIORef $ seStaticEssencesRef sEnv

      dynEsss   <- readIORef $ deEssencesRef dEnv

      length props `shouldBe` 67

      let genDoorEss = matEss @KB.EGenericDoor
      let posPath = matPath @('RelPath '[ KB.EPos ])
      let genDoorProps = map snd
            $ fromJust
            $ Map.lookup genDoorEss dynEsss

      posVals <- mapM
        (queryValueRefUnsafe posPath >=> readIORef)
        genDoorProps


      case posVals of
        ( PairValue _ (IntValue _ 2) (IntValue _ 8) :
          PairValue _ (IntValue _ 2) (IntValue _ 5) :
          [] ) -> pure ()
        _ -> error "fail"

      length posVals `shouldBe` 2

    xit "Effect triggering test" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      1 `shouldBe` 2

