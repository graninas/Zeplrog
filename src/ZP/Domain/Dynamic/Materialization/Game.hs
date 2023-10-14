{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Materialization.Game where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Static.Materialization ()
import ZP.Domain.Static.Materialization.Materializer
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
  -> [SMod.EssenceVL]
  -> ((Int, Int), Char)
  -> SMod.PropertyVL
prepareProp pathToPos statProps ((x, y), ch) =
  case Map.lookup [ch] statProps of
    Nothing -> error $ "Static property not found for symbol: "
                    <> show ch
    Just statProp ->
      error "not implemented!! put x y into prop"
      -- TODO   ------------------------------- !!!
      statProp

instance
  ( DMat p SMod.WorldVL World
  , DMat p SMod.PropertyVL Property
  , DMat [SMod.EssenceVL] SMod.ObjectVL Object
  ) =>
  DMat p SMod.GameVL Game where
  dMat _ p (SMod.GameEnvironment
              statWorld
              (SMod.IconPath pathToIcon)
              (SMod.PosPath pathToPos)
              statProps
              statObjs) = do
    let SMod.WorldData statWD = statWorld


    error "TODO: derived propertiessss!!!"



    -- N.B., repeated props will be droped.
    let iconsToStatPropsMap = Map.fromList
          [ (fromJust mbIcon, statProp)
          | statProp <- statProps
          , let mbIcon = SQuery.queryStringValue pathToIcon statProp
          , isJust mbIcon
          ]

    let cells = worldDataToList statWD

    let preparedStatProps =
          [ prepareProp pathToPos iconsToStatPropsMap cell
          | cell <- cells
          ]

    world <- dMat False p statWorld

    propsFromWorld <- mapM (dMat False p) preparedStatProps
    objsFromWorld  <- mapM spawnObject propsFromWorld
    objs           <- mapM (dMat False pathToPos) statObjs

    -- TODO: verify that all objects are in the world's bounds

    let allObjs1 = foldr (\obj m ->
          Map.insert (objectId obj) obj m)
          Map.empty
          objsFromWorld

    let allObjs2 = foldr (\obj m ->
          Map.insert (objectId obj) obj m)
          allObjs1
          objs

    allObjsVar <- newTVarIO allObjs2

    objIdVar      <- asks deObjectIdVar
    propIdVar     <- asks dePropertyIdVar
    statPropsVar  <- asks $ seStaticPropertiesVar . deSEnv
    statEsssVar   <- asks $ seStaticEssencesVar . deSEnv
    statProps     <- readTVarIO statPropsVar
    statEsss      <- readTVarIO statEsssVar

    pure $ Game
      world
      propIdVar
      objIdVar
      statProps
      statEsss
      allObjsVar
