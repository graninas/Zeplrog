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
  queryValue [] _ = Nothing
  queryValue _ (TagPropRef _) =
    error "queryValue not implemented for TagPropRef"
  queryValue (ess1 : ess2 : path) (PropDict group kvs _) = let
    propMatch = getEssence group == ess1
    kvsFound = filter (\kv -> getEssence kv == ess2) kvs
    in case (propMatch, kvsFound) of
          (False, _)   -> Nothing
          (_, [])      -> Nothing
          (True, kv:_) -> queryValue (ess2 : path) kv
  queryValue _ _ = Nothing
    -- TODO: resilent or rejecting??
    -- error "queryValue: ess path is malformed"

instance QueryValue PropertyKeyValueVL where
  queryValue (ess:path) (PropKeyVal ess1 own)
    | ess == ess1 = queryValue path own
    | otherwise = Nothing
  queryValue _ (PropKeyVal _ _) = Nothing
  queryValue [] (PropKeyBag _ _) = Nothing
    -- TODO: resilent or rejecting??
    -- error "queryValue PropKeyBag path is empty"
  queryValue (ess : path) (PropKeyBag ess1 props) = let
    kbMatch = ess == ess1
    propsFound = filter (\p -> getEssence p == ess) props
    in case (kbMatch, propsFound) of
          (False, _)         -> Nothing
          (_, [])            -> Nothing
          (True, (prop : _)) -> queryValue path prop
  queryValue _ _ = Nothing
    -- TODO: resilent or rejecting??
    -- error "queryValue PropKeyBag path is malformed"

instance QueryValue PropertyOwningVL where
  queryValue [] (OwnVal (GenericValue _ dVal)) = Just dVal
  queryValue path (OwnProp prop) = queryValue path prop
  queryValue path (SharedProp prop) = queryValue path prop
  queryValue _  _ = Nothing
    -- TODO: resilent or rejecting??
    -- error "queryValue OwnVal path not empty"

instance QueryValue (GenericValDefVL tag) where
  queryValue [] (GenericValue _ dVal) = Just dVal
  queryValue _ _ = Nothing
    -- TODO: resilent or rejecting??
    -- error "queryValue (GenericValDefVL tag) path is not empty"
