{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}

module ZP.Domain.Dynamic.Query where

import ZP.Prelude

import ZP.Domain.Dynamic.Model

import qualified Data.Map.Strict as Map
import Unsafe.Coerce (unsafeCoerce)


class QueryValueRef item where
  queryValueRef
    :: item
    -> EssencePath
    -> IO (IORef DValue)


instance QueryValueRef Property where
  queryValueRef (TagPropRef _) _ = error "queryValueRef: TagPropRef not yet implemented"
  queryValueRef _ [] = error "queryValueRef: Path is empty"
  queryValueRef (Prop _ _ _ fieldsRef _) (ess : esss) = do
    fields <- readIORef fieldsRef
    case Map.lookup ess fields of
      Nothing    -> error $ show $ "queryValueRef: ess not found: " <> ess
      Just field -> queryValueRef field esss

instance QueryValueRef PropertyOwning where
  queryValueRef (OwnVal valRef) [] = pure valRef
  queryValueRef (OwnVal _) esss = error $ show $ "Path exceeds hierarchy: " <> show esss
  queryValueRef (SharedProp prop) _ =
    error "queryValueRef (SharedProp prop): not yet implemented"
  queryValueRef (OwnDict dictRef) [] =
    error "queryValueRef (OwnDict dictRef): path is empty"
  queryValueRef (OwnDict dictRef) (e:esss) = do
    dict <- readIORef dictRef
    case Map.lookup e dict of
      Nothing -> error $ show $ "Field not found: " <> e
      Just prop -> queryValueRef prop esss
  queryValueRef (OwnProp prop) esss = queryValueRef prop esss


readBoolVal
  :: Property
  -> [Essence]
  -> IO Bool
readBoolVal prop esss = do
  valRef <- queryValueRef prop esss
  val <- readIORef valRef
  case val of
    BoolValue boolVal -> pure boolVal
    _ -> error "readBoolVal: not a bool value"

readStringVal
  :: Property
  -> [Essence]
  -> IO String
readStringVal prop esss = do
  valRef <- queryValueRef prop esss
  val <- readIORef valRef
  case val of
    StringValue val -> pure val
    _ -> error "readStringVal: not a string value"

readPathVal
  :: Property
  -> [Essence]
  -> IO [Essence]
readPathVal prop esss = do
  valRef <- queryValueRef prop esss
  val <- readIORef valRef
  case val of
    PathValue val -> pure val
    _ -> error $ "readPathVal: not a path value "

updateValue
  :: Property
  -> [Essence]
  -> DValue
  -> IO ()
updateValue prop path newVal = do
  valRef <- queryValueRef prop path
  writeIORef valRef newVal


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
