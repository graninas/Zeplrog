{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Instantiation.Game where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Static.Materialization ()
import ZP.Domain.Static.Materialization.Materializer
import qualified ZP.Domain.Static.Query as SQuery
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Instantiation.Instantiator
import ZP.Domain.Dynamic.Instantiation.Common
import ZP.Domain.Dynamic.Instantiation.Property
import ZP.Domain.Dynamic.Instantiation.Effect
import ZP.Domain.Dynamic.Instantiation.World

import Data.Proxy
import qualified Data.Map.Strict as Map
import Data.Maybe


-- Instatiation of Game

prepareProp
  :: SMod.PosEssencePathVL
  -> Map.Map String SMod.PropertyVL
  -> ((Int, Int), Char)
  -> DInstantiator (Maybe SMod.PropertyVL)
prepareProp (SMod.PosPath pathToPos) statProps ((x, y), ch) = do
  case Map.lookup [ch] statProps of
    Nothing -> do
      dTraceDebug (pure $ "Static property not found for symbol: "
                    <> show ch
                    <> ". Skipping.\nKnown static props: "
                    <> show (Map.keys statProps))
      pure Nothing
    Just statProp -> pure $ Just statProp

instance
  ( DInst () SMod.WorldVL World
  , DInst () SMod.PropertyVL Property
  , DInst SMod.PosEssencePathVL SMod.ObjectVL Object
  ) =>
  DInst () SMod.GameVL Game where
  dInst _ () (SMod.GameEnvironment
              statWorld
              (SMod.IconPath pathToIconRel)
              pathToPosRel
              statProps
              statObjs) = do
    let SMod.WorldData statWD = statWorld

    print $ "How many stat props for the game: "
      <> show (length statProps)

    -- N.B., repeated props will be droped.
    let iconsToStatPropsMap = Map.fromList
          [ ( case mbIcon of
                Just (StringValue _ icon) -> icon
                _ -> error "invalid mbIcon"
            , statProp)
          | statProp <- statProps
          , let mbIcon = SQuery.queryValueRel pathToIconRel statProp
          , case mbIcon of
              Just (StringValue _ _) -> True
              _ -> False
          ]

    -- World cells to traverse and search for icons.
    let cells = worldDataToList statWD

    -- Static properties that correspond to icons.
    preparedStatProps <-
      mapM (prepareProp pathToPosRel iconsToStatPropsMap) cells

    world <- dInst False () statWorld

    propsFromWorld <- mapM (dInst False ()) $ catMaybes preparedStatProps
    objsFromWorld  <- mapM spawnObject propsFromWorld

    -- Spawning the list of objects with world positions.
    objs <- mapM (dInst False pathToPosRel) statObjs

    -- TODO: verify that all objects are in the world's bounds

    let allObjs1 = foldr (\obj m ->
          Map.insert (objectId obj) obj m)
          Map.empty
          objsFromWorld

    let allObjs2 = foldr (\obj m ->
          Map.insert (objectId obj) obj m)
          allObjs1
          objs

    allObjsRef <- newIORef allObjs2

    objIdRef      <- asks deObjectIdRef
    propIdRef     <- asks dePropertyIdRef
    statPropsRef  <- asks $ seStaticPropertiesRef . deSEnv
    statEsssRef   <- asks $ seStaticEssencesRef . deSEnv
    statProps     <- readIORef statPropsRef
    statEsss      <- readIORef statEsssRef

    pure $ Game
      world
      propIdRef
      objIdRef
      statProps
      statEsss
      allObjsRef
