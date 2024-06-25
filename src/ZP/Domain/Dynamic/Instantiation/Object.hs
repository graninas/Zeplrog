{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Instantiation.Object where

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
import qualified ZP.Domain.Hardcode.KnowledgeBase as KB

import Data.Proxy
import qualified Data.Map.Strict as Map
import Data.Maybe


-- Materialization of Object with positions

instance
  ( DInst () SMod.EssenceVL Essence
  ) =>
  DInst SMod.PosEssencePathVL SMod.ObjectVL Object where
  dInst _ (SMod.PosPath pathToPos) (SMod.Obj x y statObjProp) = do
    essPath <- mapM (dInst False ()) pathToPos
    objProp <- dInst False () statObjProp

    let posVal = PairValue (IntValue x, IntValue y)

    updateValue essPath posVal objProp

    spawnObject objProp



updateValue :: [Category] -> Value -> Property -> DInstantiator ()
updateValue [] _ _ =
  error $ "Can't update value, path is empty."

updateValue cs'@(_:_:[]) _ (ValueProperty _ _ _) =
  error $ "Can't update value, the path is too long " <> show cs'

updateValue cs'@(c:[]) val (ValueProperty propId _ valVar) = do
  propsVar <- asks dePropertiesVar
  props    <- readTVarIO propsVar
  case Map.lookup propId props of
    Just (ess', _) | ess' == c -> atomically $ writeTVar valVar val
                   | otherwise ->
      error $ "Invalid value property. Expected: " <> show c <> " but got: " <> show ess'
    _ -> error $ "Value property not found: " <> show c

updateValue cs'@(c:cs) val prop = do
  let parentMapTVar = pPropertyBagsVar prop
  parentMap <- readTVarIO parentMapTVar

  case (Map.lookup c parentMap, cs) of
    -- Parent prop doesn't contain this category.
    (Nothing, _) -> do
      error $ "Can't update value, path points to absent children: " <> show cs'

    -- Parent prop contains a single property under the top category.
    (Just (SingleProperty owning), _) ->
      updateValueForOwning cs' val owning

    -- Parent prop contains a dict under the top category, but the path is short.
    (Just (PropertyDict _), []) -> do
      error $ "Can't update value for PropertyDict, the category exists, but path is too short: " <> show cs'

    -- Parent prop contains a dict under the top category, and the path has more elements to go.
    (Just (PropertyDict dictTVar), _) ->
      updateValueForDict cs val dictTVar


updateValueForDict [] _ _ =
  error $ "Can't update value for dict, the path is empty."
updateValueForDict cs'@(c:cs) val dictTVar = do
  dict <- readTVarIO dictTVar
  case (Map.lookup c dict, cs) of
    -- Dict doesn't contain this category.
    (Nothing, _) ->
      error $ "Can't update value for dict, the category is absent. Path: " <> show cs'

    -- Dict contains this category but the path is too short.
    (Just _, []) ->
      error $ "Can't update value for dict, the category exists but the path is too short: " <> show cs'

    -- Dict contains this category and the path is good.
    (Just owning, _) -> updateValueForOwning cs' val owning


updateValueForOwning [] _ _ =
  error $ "Can't update value for owning, the path is empty."
updateValueForOwning cs val (OwnProperty prop) =
  updateValue cs val prop
updateValueForOwning cs _ (SharedProperty _) =
  error $ "Can't update value for shared property: " <> show cs

-- addChildProperty :: [Category] -> Property -> Property -> DInstantiator ()
-- addChildProperty [] _ _ =
--   error $ "Can't add a child under for property, path is empty."
-- addChildProperty cs'@(_:_:[]) _ (ValueProperty _ _ _) =
--   error $ "Can't add a child under a ValueProperty. Also, the path is too long " <> show cs'
-- addChildProperty cs'@(_:[]) _ (ValueProperty _ _ _) =
--   error $ "Can't add a child under a ValueProperty. Current path: " <> show cs'
-- addChildProperty cs'@(c:cs) childProp prop = do
--   let parentMapTVar = pPropertyBagsVar prop
--   parentMap <- readTVarIO parentMapTVar

--   case (Map.lookup c parentMap, cs) of
--     -- Parent prop doesn't contain this category, and the path is good.
--     (Nothing, []) -> do
--       let parentMap' = Map.insert c (SingleProperty $ OwnProperty childProp) parentMap
--       atomically $ writeTVar parentMapTVar parentMap'

--     -- Parent prop doesn't contain this category, but the path is too long.
--     (Nothing, _) -> do
--       error $ "Can't add a child, path points to absent children: " <> show cs'

--     -- Parent prop contains a single property under the top category.
--     (Just (SingleProperty _), _) ->
--       error $ "Can't add a child under a SingleProperty: " <> show cs'

--     -- Parent prop contains a dict under the top category, but the path is short.
--     (Just (PropertyDict _), []) -> do
--       error $ "Can't add a child under a PropertyDict, the category exists, but path is too short: " <> show cs'

--     -- Parent prop contains a dict under the top category, and the path has more elements to go.
--     (Just (PropertyDict dictTVar), _) ->
--       addChildPropertyForDict cs childProp dictTVar


-- addChildPropertyForDict [] _ _ =
--   error $ "Can't add a child under a property dict, the path is empty."
-- addChildPropertyForDict cs'@(c:cs) childProp dictTVar = do
--   dict <- readTVarIO dictTVar
--   case (Map.lookup c dict, cs) of
--     -- Dict doesn't contain this category and the path is good.
--     (Nothing, []) -> do
--       let dict' = Map.insert c (OwnProperty childProp) dict
--       atomically $ writeTVar dictTVar dict'

--     -- Dict doesn't contain this category but there are more steps to go.
--     (Nothing, _) ->
--       error $ "Can't add a child under a property dict, the category is absent but the path is too long: " <> show cs'

--     -- Dict contains this category but the path is too short.
--     (Just _, []) ->
--       error $ "Can't add a child under a property dict, the category exists but the path is too short: " <> show cs'

--     -- Dict contains this category and the path is good.
--     (Just owning, _) -> addChildPropertyForOwning cs childProp owning


-- addChildPropertyForOwning [] _ _ =
--   error $ "Can't add a child for property owning, the path is empty."
-- addChildPropertyForOwning cs childProp (OwnProperty prop) =
--   addChildProperty cs childProp prop
-- addChildPropertyForOwning cs _ (SharedProperty _) =
--   error $ "Can't add a child for shared property: " <> show cs
