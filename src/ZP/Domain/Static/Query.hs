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
queryStringValue _ (DerivedProp _ _ _) =
  error "queryStringValue not implemented for DerivedProp"
queryStringValue _ (PropScript _ _) = Nothing
queryStringValue (ess:esss) (PropDict root kvs) = let
  ess' = getEssence root
  in case ess == ess' of
        True -> queryStringForKeyVals esss kvs
        False -> Nothing

-- Hardcoded function.
-- TODO: move to the Query language.
queryStringForKeyVals
  :: EssencePathVL
  -> [PropertyKeyValueVL]
  -> Maybe String
queryStringForKeyVals [] _ = Nothing
queryStringForKeyVals _ [] = Nothing
queryStringForKeyVals (ess:esss) (PropKeyVal ess' owning : kvs)
  | ess == ess' = queryStringForOwning esss owning
  | otherwise = queryStringForKeyVals (ess:esss) kvs
queryStringForKeyVals (ess:esss) (PropKeyBag ess' ownings : kvs)
  | ess == ess' = queryStringForOwnings esss ownings
  | otherwise = queryStringForKeyVals (ess:esss) kvs

-- Hardcoded function.
-- TODO: move to the Query language.
queryStringForOwning
  :: EssencePathVL
  -> PropertyOwningVL
  -> Maybe String
queryStringForOwning esss (OwnProp prop) =
  queryStringValue esss prop
queryStringForOwning esss (SharedProp prop) =
  queryStringValue esss prop

-- Hardcoded function.
-- TODO: move to the Query language.
queryStringForOwnings
  :: EssencePathVL
  -> [PropertyOwningVL]
  -> Maybe String
queryStringForOwnings [] _ = Nothing
queryStringForOwnings _ [] = Nothing
queryStringForOwnings esss (owning : ownings) =
  case queryStringForOwning esss owning of
    Nothing  -> queryStringForOwnings esss ownings
    Just str -> Just str
