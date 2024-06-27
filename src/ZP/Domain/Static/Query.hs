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


getComboPropertyId :: PropertyGroupVL -> (EssenceVL, StaticPropertyId)
getComboPropertyId (GroupId ess sId)       = (ess, sId)
getComboPropertyId (GroupRootId ess sId _) = (ess, sId)

getEssenceFromKV :: PropertyKeyValueVL -> EssenceVL
getEssenceFromKV (PropKeyBag ess _) = ess
getEssenceFromKV (PropKeyVal ess _) = ess

getGroup :: PropertyVL -> PropertyGroupVL
getGroup (PropDict group _ _) = group
getGroup (TagPropRef _) = error "getGroup not implemented for TagPropRef"

getEssence :: PropertyVL -> EssenceVL
getEssence (PropDict group _ _) = fst $ getComboPropertyId group
getEssence (TagPropRef _) = error "getEssence not implemented for TagPropRef"

getTagPropEss :: TagPropertyVL -> EssenceVL
getTagPropEss (TagProp tagGroup) = getTagPropGroupEssence tagGroup

getTagPropGroupEssence :: TagPropertyGroupVL -> EssenceVL
getTagPropGroupEssence (TagGroup ess) = ess
getTagPropGroupEssence (TagGroupRoot ess _) = ess


class QueryValue from where
  queryValue :: EssencePathVL -> from -> Maybe DValue

instance QueryValue PropertyVL where
  queryValue [] prop = Nothing
  queryValue _ (TagPropRef _) =
    error "queryValue StringTag not implemented for TagPropRef"
  queryValue (ess : path) (PropDict _ kvs _) =
    case filter (\kv -> getEssenceFromKV kv == ess) kvs of
      [] -> Nothing
      (kv:_) -> queryValue path kv

instance QueryValue PropertyKeyValueVL where
  queryValue path (PropKeyVal _ own) = queryValue path own
  queryValue [] (PropKeyBag _ _) = error "QueryValue PropKeyBag path is empty"
  queryValue (ess : path) (PropKeyBag _ props) =
    case filter (\p -> getEssence p == ess) props of
      [] -> Nothing
      (prop : _) -> queryValue path prop

instance QueryValue PropertyOwningVL where
  queryValue [] (OwnVal (GenericValue _ dVal)) = Just dVal
  queryValue _  (OwnVal _) = error "QueryValue OwnVal path not empty"
  queryValue path (OwnProp prop) = queryValue path prop
  queryValue path (SharedProp prop) = queryValue path prop

instance QueryValue (GenericValDefVL tag) where
  queryValue [] (GenericValue _ dVal) = Just dVal
  queryValue _ _ = error "QueryValue (GenericValDefVL tag) path is not empty"
