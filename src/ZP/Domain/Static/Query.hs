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
queryStringValue _ (StaticProp _) =
  error "queryStringValue not implemented for StaticProp"
queryStringValue _ (StaticPropRef _) =
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
        True -> queryStringValueForKeyVals esss kvs
        False -> Nothing

-- Hardcoded function.
-- Queries a value of string for this property.
-- The path doesn't contain the essence of this property.
-- TODO: move to the Query language.
queryStringValueRelative :: EssencePathVL -> PropertyVL -> Maybe String
queryStringValueRelative [] _ = Nothing
queryStringValueRelative _ (StaticProp _) =
  error "queryStringValueRelative not implemented for StaticProp"
queryStringValueRelative _ (StaticPropRef _) =
  error "queryStringValueRelative not implemented for StaticPropRef"
queryStringValueRelative [] (PropVal root valDef) =
  getStringValue valDef
queryStringValueRelative esss (PropVal _ _) =
  error $ "queryStringValueRelative: path is not empty: " <> show esss
queryStringValueRelative _ (DerivedProp _ _ _) =
  error "queryStringValueRelative not implemented for DerivedProp"
queryStringValueRelative _ (PropScript _ _) = Nothing
queryStringValueRelative esss (PropDict root kvs) =
  queryStringValueForKeyVals esss kvs

-- Hardcoded function.
-- TODO: move to the Query language.
queryStringValueForKeyVals
  :: EssencePathVL
  -> [PropertyKeyValueVL]
  -> Maybe String
queryStringValueForKeyVals [] _ = Nothing
queryStringValueForKeyVals _ [] = Nothing
queryStringValueForKeyVals path@(ess:_) (PropKeyVal ess' owning : kvs)
  | ess == ess' = queryStringValueForOwning path owning
  | otherwise = queryStringValueForKeyVals path kvs
queryStringValueForKeyVals path@(ess:_) (PropKeyBag ess' ownings : kvs)
  | ess == ess' = queryStringValueForOwnings path ownings
  | otherwise = queryStringValueForKeyVals path kvs

-- Hardcoded function.
-- TODO: move to the Query language.
queryStringValueForOwning
  :: EssencePathVL
  -> PropertyOwningVL
  -> Maybe String
queryStringValueForOwning esss (OwnProp prop) =
  queryStringValue esss prop
queryStringValueForOwning esss (SharedProp prop) =
  queryStringValue esss prop

-- Hardcoded function.
-- TODO: move to the Query language.
queryStringValueForOwnings
  :: EssencePathVL
  -> [PropertyOwningVL]
  -> Maybe String
queryStringValueForOwnings [] _ = Nothing
queryStringValueForOwnings _ [] = Nothing
queryStringValueForOwnings esss (owning : ownings) =
  case queryStringValueForOwning esss owning of
    Nothing  -> queryStringValueForOwnings esss ownings
    Just str -> Just str
