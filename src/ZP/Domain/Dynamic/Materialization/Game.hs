{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Materialization.Game where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import qualified ZP.Domain.Static.Query as SQuery
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Materialization.Materializer
import ZP.Domain.Dynamic.Materialization.Common
import ZP.Domain.Dynamic.Materialization.Property
import ZP.Domain.Dynamic.Materialization.Effect
import ZP.Domain.Dynamic.Materialization.World

import Data.Proxy
import qualified Data.Map.Strict as Map
import Data.Maybe


-- Materialization of Game

prepareProp
  :: Map.Map String SMod.PropertyVL
  -> ((Int, Int), Char)
  -> SMod.PropertyVL
prepareProp statProps ((x, y), ch) =
  case Map.lookup [ch] statProps of
    Nothing -> error $ "Static property not found for symbol: " <> [ch]
    Just statProp ->
      -- TODO   ------------------------------- !!!
      statProp


spawnObject :: TVar ObjectId -> Property -> DMaterializer Object
spawnObject objIdVar prop = atomically $ do
  objId <- readTVar objIdVar
  writeTVar objIdVar $ objId + 1
  pure $ Object objId prop


instance
  DMat p SMod.GameVL Game where
  dMat _ p (SMod.GameEnvironment statWorld pathToIcon statProps statObjs) = do
    let SMod.WorldData statWD = statWorld

    -- N.B., repeated props will be droped.
    let iconsToStatPropsMap = Map.fromList
          [ (fromJust mbIcon, statProp)
          | statProp <- statProps
          , let mbIcon = SQuery.queryStringValue pathToIcon statProp
          , isJust mbIcon
          ]

    let cells = worldDataToList statWD

    let preparedStatProps =
          [ prepareProp iconsToStatPropsMap cell
          | cell <- cells
          ]

    world <- dMat False p statWorld

    DEnv sEnv propIdVar objIdVar _ <- ask
    let SMat.SEnv _ _ statPropsVar statEsssVar = sEnv

    propsFromWorld <- mapM (dMat False p) preparedStatProps
    objsFromWorld  <- mapM (spawnObject objIdVar) propsFromWorld
    objs           <- mapM (dMat False p) statObjs

    let allObjs1 = foldr (\obj m ->
          Map.insert (objectId obj) obj m)
          Map.empty
          objsFromWorld

    let allObjs2 = foldr (\obj m ->
          Map.insert (objectId obj) obj m)
          allObjs1
          objs

    allObjsVar <- newTVarIO allObjs2

    pure $ Game
      world
      propIdVar
      objIdVar
      statPropsVar
      statEsssVar
      allObjsVar

