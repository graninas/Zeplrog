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
      if tn1 == tn2 || tn2 == tagName DPlaceholder
        then writeIORef valRef newVal
        else error $ "Value types mismatch: " <> show tn1 <> " " <> show tn2

