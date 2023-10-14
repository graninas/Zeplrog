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

-- TODO: static materialization of abstract props

-- Materialization of Game

prepareProp
  :: SMod.PosEssencePathVL
  -> Map.Map String SMod.PropertyVL
  -> ((Int, Int), Char)
  -> (SMat.Instantiate, SMod.PropertyVL)
prepareProp (SMod.PosPath pathToPos) statProps ((x, y), ch) = do
  case Map.lookup [ch] statProps of
    Nothing -> error $ "Static property not found for symbol: "
                    <> show ch
                    <> "\nKnown static props: "
                    <> show (Map.keys statProps)
    Just statProp ->
      ( SMat.InstantiateValue pathToPos
          $ SMod.PairValue
            (SMod.IntValue x)
            (SMod.IntValue y)
      , statProp)

instance
  ( DMat () SMod.WorldVL World
  , DMat () SMod.PropertyVL Property
  , DMat SMod.PosEssencePathVL SMod.ObjectVL Object
  , DMat () (SMat.Instantiate, SMod.PropertyVL) Property
  ) =>
  DMat () SMod.GameVL Game where
  dMat _ () (SMod.GameEnvironment
              statWorld
              (SMod.IconPath pathToIcon)
              pathToPos
              statProps
              statObjs) = do
    let SMod.WorldData statWD = statWorld

    print $ "How many stat props for the game: "
      <> show (length statProps)

    -- N.B., repeated props will be droped.
    let iconsToStatPropsMap = Map.fromList
          [ (fromJust mbIcon, statProp)
          | statProp <- statProps
          , let mbIcon = SQuery.queryStringValueRelative pathToIcon statProp
          , isJust (trace ("\n\n" <> show mbIcon) mbIcon)
          ]

    -- World cells to traverse and search for icons.
    let cells = worldDataToList statWD

    -- Static properties that correspond to icons.
    -- Positions are not embedded into the props.
    let preparedStatProps =
          map (prepareProp pathToPos iconsToStatPropsMap) cells

    world <- dMat False () statWorld

    propsFromWorld <- mapM (dMat False ()) preparedStatProps
    objsFromWorld  <- mapM spawnObject propsFromWorld

    -- Spawning the list of objects with world positions.
    objs <- mapM (dMat False pathToPos) statObjs

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
