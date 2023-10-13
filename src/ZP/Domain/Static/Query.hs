module ZP.Domain.Static.Query where

import ZP.Prelude

import ZP.Domain.Static.Model.Common
import ZP.Domain.Static.Model.Property
import ZP.Domain.Static.Model.Effect
import ZP.Domain.Static.Model.World
import ZP.Domain.Static.Model.Script



getEssence :: PropertyRootVL -> EssenceVL
getEssence (EssRoot ess) = ess
getEssence (PropRoot ess _) = ess

getEssenceFromKV :: PropertyKeyValueVL -> EssenceVL
getEssenceFromKV (PropKeyBag ess _) = ess
getEssenceFromKV (PropKeyVal ess _) = ess

getRoot :: PropertyVL -> PropertyRootVL
getRoot (StaticProp root) = root
getRoot (StaticPropRef prop) = getRoot prop
getRoot (PropVal root _) = root
getRoot (PropDict root _) = root
getRoot (PropScript root _) = root


getStringValue :: ValDefVL -> Maybe String
getStringValue (StringValue str) = Just str
getStringValue _ = Nothing


-- Hardcoded function.
-- TODO: move to the Query language.
queryStringValue :: EssencePathVL -> PropertyVL -> Maybe String
queryStringValue [] _ = Nothing
queryStringValue (ess:esss) (StaticProp _) =
  error "queryStringValue not implemented for StaticProp"
queryStringValue (ess:esss) (StaticPropRef _) =
  error "queryStringValue not implemented for StaticPropRef"
queryStringValue (ess:[]) (PropVal root valDef) = let
  ess' = getEssence root
  in if ess == ess'
        then getStringValue valDef
        else Nothing
queryStringValue _ (PropVal _ _) = Nothing
queryStringValue _ (DerivedProperty _ _ _) =
  error "queryStringValue not implemented for DerivedProperty"
queryStringValue _ (PropScript _) = Nothing
queryStringValue (ess:esss) (PropDict root kvs) = let
  ess' = getEssence root
  in case ess == ess' of
        True -> queryStringValue2 esss kvs
        False -> Nothing

-- Hardcoded function.
-- TODO: move to the Query language.
queryStringValue2
  :: EssencePathVL
  -> [PropertyKeyValueVL]
  -> Maybe String
queryStringValue2 [] _ = Nothing
queryStringValue2 _ [] = Nothing
queryStringValue2 (ess:esss) (PropKeyVal ess' owning : kvs)
  | ess == ess' = queryStringValue3 esss owning
  | otherwise = queryStringValue2 (ess:esss) kvs
queryStringValue2 (ess:esss) (PropKeyBag ess' ownings : kvs)
  | ess == ess' = queryStringValue4 esss ownings
  | otherwise = queryStringValue2 (ess:esss) kvs

-- Hardcoded function.
-- TODO: move to the Query language.
queryStringValue3
  :: EssencePathVL
  -> PropertyOwningVL
  -> Maybe String
queryStringValue3 esss (OwnProp prop) =
  queryStringValue esss prop
queryStringValue3 esss (SharedProp prop) =
  queryStringValue esss prop

-- Hardcoded function.
-- TODO: move to the Query language.
queryStringValue4
  :: EssencePathVL
  -> [PropertyOwningVL]
  -> Maybe String
queryStringValue4 [] _ = Nothing
queryStringValue4 _ [] = Nothing
queryStringValue4 esss (owning : ownings) =
  case queryStringValue3 esss owning of
    Nothing  -> queryStringValue4 esss ownings
    Just str -> Just str
