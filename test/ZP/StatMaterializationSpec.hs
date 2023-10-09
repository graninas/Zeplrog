{-# LANGUAGE DataKinds #-}

module ZP.StatMaterializationSpec where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materializer
import ZP.Domain.Static.Materialization
import qualified ZP.Assets.KnowledgeBase as KB

import Test.Hspec

import Data.Proxy
import qualified Data.Map as Map


type HPValOwnProp     = OwnProp (KB.HPVal 100)
type PosValSharedProp = SharedProp (KB.PosConst 3 5)
type DoorStateProp    = OwnProp KB.OpenStateRef


matDoor :: Materializer (Property 'ValueLevel)
matDoor = mat $ Proxy @KB.Door


-- Manually materialized door

matDoorCustom :: Materializer (Property 'ValueLevel)
matDoorCustom = do
  root <- mat $ Proxy @(EssRoot KB.EDoor)

  ehp       <- mat $ Proxy @KB.EHP
  hpPropOwn <- mat $ Proxy @HPValOwnProp

  ePos          <- mat $ Proxy @KB.EPos
  posPropShared <- mat $ Proxy @PosValSharedProp

  -- eState   <- mat $ Proxy @KB.EState
  -- stateOwn <- mat $ Proxy @DoorStateProp

  let props =
        [ PropKeyVal ehp hpPropOwn
        , PropKeyVal ePos posPropShared
        -- , (eState, stateOwn)
        ]

  pure $ PropDict root props

spec :: Spec
spec = do
  describe "Static materialization tests" $ do
    it "Door materialization test" $ do
      (_, door) <- runMaterializer matDoor
      case door of
        PropDict root props -> do
          -- TODO: more tests of the prop
          length props `shouldBe` 5
        _ -> error "invalid materialization result"

  -- describe "Materialization test" $ do
  --   it "Materialization: value prop" $ do
  --     (_, DynamicProperty _ _ propsVar valVar) <- runMaterializer matDoorCustom

  --     mbVal <- readTVarIO valVar
  --     case mbVal of
  --       Nothing -> pure ()
  --       Just _  -> error "Unexpected value"

  --     ehp <- mat' $ Proxy @KB.EHP

  --     props <- readTVarIO propsVar
  --     case Map.lookup ehp props of
  --       Nothing -> error "Unexpected nothing"
  --       Just (OwnDynamicProperty (DynamicProperty _ _ _ valVar2)) -> do
  --         mbVal <- readTVarIO valVar2
  --         case mbVal of
  --           Nothing -> error "Unexpected no value"
  --           Just (VarValue valVar3) -> do
  --             val3 <- readTVarIO valVar3
  --             val3 `shouldBe` IntValue 100
  --           Just _ -> error "Invalid val"
  --       Just _ -> error "Invalid dyn prop"

  --   it "Materialization: 1 shared prop" $ do
  --     (Env spsVar, _) <- runMaterializer matDoorCustom
  --     sps <- readTVarIO spsVar
  --     -- TODO: verify the prop itself
  --     Map.size sps `shouldBe` 1

  --   it "Full materialization" $ do
  --     (Env spsVar, _) <- runMaterializer matDoor
  --     sps <- readTVarIO spsVar
  --     -- TODO: verify the prop itself
  --     Map.size sps `shouldBe` 1

  -- describe "Static properties test" $ do
  --   it "Static prop is accessible from a dynamic prop 1" $ do
  --     (_, DynamicProperty _ statPropRef _ _) <- runMaterializer matDoorCustom
  --     let ess = Browser.browseDyn Browser.GetEssence statPropRef
  --     ess `shouldBe` "object:door"

  --   it "Static prop is accessible from a dynamic prop 2" $ do
  --     (_, DynamicProperty _ _ propsVar _) <- runMaterializer matDoorCustom
  --     props <- readTVarIO propsVar
  --     ehp <- mat' $ Proxy @KB.EHP
  --     case Map.lookup ehp props of
  --       Nothing -> error "Unexpected nothing"
  --       Just (OwnDynamicProperty (DynamicProperty _ statPropRef _ _)) -> do
  --         let ess = Browser.browseDyn Browser.GetEssence statPropRef
  --         ess `shouldBe` "intrinsics:hp"
