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


class QueryValueRef it where
  queryValueRef
    :: DEssencePath
    -> it
    -> IO (Maybe (IORef DValue))


instance QueryValueRef Property where
  queryValueRef (DAbsPath []) _ = pure Nothing
  queryValueRef (DRelPath []) _ = pure Nothing

  queryValueRef _ (TagPropRef _ _) =
    error "queryValueRef not implemented for TagPropRef"

  queryValueRef (DAbsPath (ess1 : ess2 : path))
                (Prop _ ess _ _ fieldsRef _) = do
    fields <- readIORef fieldsRef
    let propMatch = ess == ess1
    let mbOwning = Map.lookup ess2 fields
    case (propMatch, mbOwning) of
          (False, _)   -> pure Nothing
          (_, Nothing)      -> pure Nothing
          (True, Just owning) -> queryValueRef (DRelPath path) owning

  queryValueRef (DRelPath (ess1 : path))
                (Prop _ _ _ _ fieldsRef _) = do
    fields <- readIORef fieldsRef
    let mbOwning = Map.lookup ess1 fields
    case mbOwning of
          Nothing     -> pure Nothing
          Just owning -> queryValueRef (DRelPath path) owning

  queryValueRef _ _ = pure Nothing

instance QueryValueRef PropertyOwning where
  queryValueRef (DAbsPath []) (OwnVal valRef) = pure $ Just valRef
  queryValueRef (DRelPath []) (OwnVal valRef) = pure $ Just valRef

  queryValueRef _ (OwnVal _) = pure Nothing

  queryValueRef (DAbsPath (ess:path)) (OwnDict dictRef) = do
    dict <- readIORef dictRef
    case Map.lookup ess dict of
      Nothing   -> pure Nothing

      ------------------------------- V ???
      Just prop -> queryValueRef (DRelPath path) prop

  queryValueRef (DRelPath path) (OwnDict dictRef) =
    error $ "queryValueRef DRelPath is invalid for OwnDict: " <> show path

  queryValueRef path (OwnProp prop) = queryValueRef path prop
  queryValueRef _ (SharedProp _) =
    error "queryValueRef not implemented for SharedProp"

queryValueRefUnsafe
  :: QueryValueRef it
  => DEssencePath
  -> it
  -> IO (IORef DValue)
queryValueRefUnsafe path it = do
  mbValRef <- queryValueRef path it
  case mbValRef of
    Nothing -> error $ "queryValueRefUnsafe value not found: " <> show path
    Just valRef -> pure valRef

------------

readBoolVal
  :: Property
  -> DEssencePath
  -> IO Bool
readBoolVal prop path = do
  mbValRef <- queryValueRef path prop
  case mbValRef of
    Nothing -> error $ "Value not found: " <> show path
    Just valRef -> do
      val <- readIORef valRef
      case val of
        BoolValue _ boolVal -> pure boolVal
        _ -> error "readBoolVal: not a bool value"

readStringVal
  :: Property
  -> DEssencePath
  -> IO String
readStringVal prop path = do
  mbValRef <- queryValueRef path prop
  case mbValRef of
    Nothing -> error $ "Value not found: " <> show path
    Just valRef -> do
      val <- readIORef valRef
      case val of
        StringValue _ val -> pure val
        _ -> error "readStringVal: not a string value"

readPathVal
  :: Property
  -> DEssencePath
  -> IO DEssencePath
readPathVal prop path = do
  mbValRef <- queryValueRef path prop
  case mbValRef of
    Nothing -> error $ "Value not found: " <> show path
    Just valRef -> do
      val <- readIORef valRef
      case val of
        PathValue _ val -> pure val
        _ -> error $ "readPathVal: not a path value "

updateValue
  :: Property
  -> DEssencePath
  -> DValue
  -> IO ()
updateValue prop path newVal = do
  mbValRef <- queryValueRef path prop
  case mbValRef of
    Nothing -> error $ "Value not found: " <> show path
    Just valRef -> do
      val <- readIORef valRef
      let tn1 = tagName newVal
      let tn2 = tagName val
      if tn1 == tn2
        then writeIORef valRef newVal
        else error $ "Value types mismatch: " <> show tn1 <> " " <> show tn2


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
