{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ZP.Domain.Static.Query where

import ZP.Prelude

import ZP.Domain.Static.Model.Common
import ZP.Domain.Static.Model.Property
import ZP.Domain.Static.Model.Effect
import ZP.Domain.Static.Model.World
import ZP.Domain.Static.Model.Script

import Unsafe.Coerce (unsafeCoerce)


class GetEssence it where
  getEssence :: it -> EssenceVL
  getComboId :: it -> (EssenceVL, StaticPropertyId)
  getEssence it = fst $ getComboId it

getGroup :: PropertyVL -> PropertyGroupVL
getGroup (PropDict group _ _) = group
getGroup (TagPropRef _) = error "getGroup not implemented for TagPropRef"

instance GetEssence PropertyKeyValueVL where
  getEssence (PropKeyBag ess _) = ess
  getEssence (PropKeyVal ess _) = ess
  getComboId _ = error "getComboId not supported for PropertyKeyValueVL"

instance GetEssence PropertyGroupVL where
  getComboId (GroupId ess sId)       = (ess, sId)
  getComboId (GroupRootId ess sId _) = (ess, sId)

instance GetEssence PropertyVL where
  getComboId (TagPropRef _) = error "getComboId not implemented for TagPropRef"
  getComboId (PropDict group _ _) = getComboId group

instance GetEssence TagPropertyVL where
  getEssence (TagProp tagGroup) = getEssence tagGroup
  getComboId _ = error "getEssgetComboId not supported for TagPropertyVL"

instance GetEssence TagPropertyGroupVL where
  getEssence (TagGroup ess) = ess
  getEssence (TagGroupRoot ess _) = ess
  getComboId _ = error "getComboId not supported for TagPropertyGroupVL"


class QueryValue from where
  queryValue :: EssencePathVL -> from -> Maybe DValue

instance QueryValue PropertyVL where
  queryValue (RelPath []) _ = Nothing
  queryValue (AbsPath []) _ = Nothing
  queryValue _ (TagPropRef _) =
    error "queryValue not implemented for TagPropRef"

  queryValue (AbsPath (ess1 : ess2 : path)) (PropDict group kvs _) = let
    propMatch = getEssence group == ess1
    kvsFound = filter (\kv -> getEssence kv == ess2) kvs
    in case (propMatch, kvsFound) of
          (False, _)   -> Nothing
          (_, [])      -> Nothing
          (True, kv:_) -> queryValue (RelPath path) kv

  queryValue (RelPath (ess1 : path)) (PropDict _ kvs _) = let
    kvsFound = filter (\kv -> getEssence kv == ess1) kvs
    in case kvsFound of
          []     -> Nothing
          (kv:_) -> queryValue (RelPath path) kv
  queryValue _ _ = Nothing

instance QueryValue PropertyKeyValueVL where
  queryValue (AbsPath []) (PropKeyVal _ _) = Nothing
  queryValue (AbsPath (ess:path)) (PropKeyVal ess1 own)
    | ess == ess1 = queryValue (AbsPath path) own
    | otherwise = Nothing

  queryValue (RelPath path) (PropKeyVal _ own) =
    queryValue (RelPath path) own

  queryValue (AbsPath []) (PropKeyBag _ _) = Nothing
  queryValue (RelPath []) (PropKeyBag _ _) = Nothing

  queryValue (AbsPath (ess : path)) (PropKeyBag ess1 props) = let
    kbMatch = ess == ess1
    propsFound = filter (\p -> getEssence p == ess) props
    in case (kbMatch, propsFound) of
          (False, _)         -> Nothing
          (_, [])            -> Nothing
          (True, (prop : _)) -> queryValue (RelPath path) prop

  queryValue (RelPath path) (PropKeyBag _ props) =
    error $ "queryValue RelPath is invalid for PropKeyBag: " <> show path

  queryValue _ _ = Nothing

instance QueryValue PropertyOwningVL where
  queryValue (AbsPath []) (OwnVal (GenericValue _ dVal)) = Just dVal
  queryValue (RelPath []) (OwnVal (GenericValue _ dVal)) = Just dVal

  queryValue path (OwnProp prop) = queryValue path prop
  queryValue path (SharedProp prop) = queryValue path prop
  queryValue _  _ = Nothing

instance QueryValue (GenericValDefVL tag) where
  queryValue (AbsPath []) (GenericValue _ dVal) = Just dVal
  queryValue (RelPath []) (GenericValue _ dVal) = Just dVal
  queryValue _ _ = Nothing


