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

kickEffectMat :: IO Effect
kickEffectMat = do
  (SMat.Env _ statPropsVar, statKickEff) <-
    SMat.runMaterializer SMat.DebugEnabled $ SMat.mat $ Proxy @KB.Kick
  statProps <- readIORef statPropsVar
  mat' statProps statKickEff

-- -- temporary
-- inRange :: (Int, Int) -> Property -> IO Bool
-- inRange pos prop = do
--   propsBag <- readTVarIO $ dpPropsBag prop
--   case propsBag of
--     SingleProperty propOwning -> do
--   case Map.lookup "intrinsics:pos" propsDict of
--     Nothing -> pure False
--     Just posProp -> do
--       mbVal <- readTVarIO $ dpPropValue posProp
--       case mbVal of
--         Just (ConstValue (PairValue pos')) -> pure $ pos == pos'
--         _ -> error "Invalid pos prop val"



-- applyEffect :: Game -> Effect -> (Int, Int) -> IO ()
-- applyEffect (Game propDict triggs) eff pos = do
--   let props = Map.elems propDict
--   inRangeProps <- filterM (inRange pos) props
--   print $ length inRangeProps
--   pure ()


spec :: Spec
spec = do
  describe "Game tests" $ do

    xit "Effect triggering test" $ do
      (SMat.Env _ statPropsVar, statGame) <-
        SMat.runMaterializer SMat.DebugEnabled gameMat
      statProps <- readIORef statPropsVar
      (Env _ _, game) <-
        runMaterializer statProps $ mat False statGame

        -- (_, kickEff) <- kickEffectMat runMaterializer statProps $ kickEffectMat
        -- res <- applyEffect game kickEff (3, 5)

      1 `shouldBe` 2

