{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Property where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Static.Model
import ZP.Domain.Static.Query
import ZP.Domain.Static.Materialization.Materializer
import ZP.Domain.Static.Materialization.Common
import ZP.Domain.Static.Materialization.Script

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


---------- Static property materialization --------------

data SrcPropKVs propKVs
data PropOwns propOwns
data Props props

type ResPropKVs = [PropertyKeyValueVL]


-- | Lookups a property having this essence.
--   If property found, it will be returned.
--   If there is no such property, it will be created.

withProperty
  :: SMat p root (EssenceVL, PropertyRootVL)
  => p
  -> Proxy root
  -> (PropertyRootVL -> SMaterializer PropertyVL)
  -> SMaterializer PropertyVL
withProperty p rootProxy matPropF = do
  SEnv dbg _ _ statEssencesVar <- ask

  (ess, root) <- sMat p rootProxy

  sTraceDebug $ "Static property to introduce: " <> show ess

  esss <- readTVarIO statEssencesVar

  case Map.lookup ess esss of
    Just (statPropId, prop) -> do
      sTraceDebug $ show ess <> ": already exists: " <> show statPropId
      pure prop
    Nothing -> do
      statPropId <- getNextStaticPropertyId
      prop <- matPropF root

      addStaticProperty (statPropId, ess, prop)

      sTraceDebug $ show ess <> ": created: " <> show statPropId

      pure prop

-- Statically materialize property root

instance
  ( SMat p ess EssenceVL
  ) =>
  SMat p ('EssRoot @'TypeLevel ess)
         (EssenceVL, PropertyRootVL) where
  sMat p _ = do
    ess <- sMat p $ Proxy @ess
    pure (ess, EssRoot ess)

instance
  ( SMat p ess EssenceVL
  , SMat p prop (EssenceVL, PropertyVL)
  ) =>
  SMat p ('PropRoot @'TypeLevel ess prop)
         (EssenceVL, PropertyRootVL) where
  sMat p _ = do
    (ess, prop) <- sMat p $ Proxy @prop
    pure (ess, PropRoot ess prop)

-- Statically materialize property

instance
  ( SMat p (SrcPropKVs propKVs) ResPropKVs
  , SMat p root (EssenceVL, PropertyRootVL)
  ) =>
  SMat p ('PropDict @'TypeLevel root propKVs)
         PropertyVL where
  sMat p _ = withProperty p (Proxy @root) $ \root -> do
    propKVs <- sMat p $ Proxy @(SrcPropKVs propKVs)
    pure $ PropDict root propKVs

-- | Merges props with preference of the first keys.
-- Does not merge internal props.
mergePropKVs :: [PropertyKeyValueVL] -> [PropertyKeyValueVL] -> [PropertyKeyValueVL]
mergePropKVs kvs1 kvs2 = let
  pKVs1 = Map.fromList [ (getEssenceFromKV k, k) | k <- kvs1]
  pKVs2 = Map.fromList [ (getEssenceFromKV k, k) | k <- kvs2]
  pKVs3 = Map.union pKVs1 pKVs2
  in Map.elems pKVs3


instance
  ( SMat p ess EssenceVL
  , SMat p abstractProp PropertyVL
  , SMat p (SrcPropKVs propKVs) ResPropKVs
  ) =>
  SMat p ('DerivedProp @'TypeLevel ess abstractProp propKVs)
         PropertyVL where
  sMat p _ = do
    ess <- sMat p $ Proxy @ess

    sTraceDebug $ "Deriving property: " <> show ess

    abstractProp <- sMat p $ Proxy @abstractProp

    case abstractProp of
      PropDict root abstractPropKVs -> do
        let abstractPropEss = getEssence root
        sTraceDebug $ "Abstract property to derive: " <> show abstractPropEss

        statPropId <- getNextStaticPropertyId

        propKVs :: [PropertyKeyValueVL] <- sMat p $ Proxy @(SrcPropKVs propKVs)
        let propKVs' = mergePropKVs propKVs abstractPropKVs

        let prop = PropDict (PropRoot ess abstractProp) propKVs'
        addStaticProperty (statPropId, ess, prop)
        sTraceDebug $ show ess <> ": new property is derived: " <> show statPropId

        pure prop

      _ -> error "abstract non-dict prop is not supported yet"

instance
  ( SMat p val ValDefVL
  , SMat p root (EssenceVL, PropertyRootVL)
  ) =>
  SMat p ('PropVal @'TypeLevel root val)
         PropertyVL where
  sMat p _ = withProperty p (Proxy @root) $ \root -> do
    val <- sMat p $ Proxy @val
    pure $ PropVal root val

instance
  ( SMat p script ScriptVL
  , SMat p root (EssenceVL, PropertyRootVL)
  ) =>
  SMat p ('PropScript @'TypeLevel root script)
         PropertyVL where
  sMat p _ = withProperty p (Proxy @root) $ \root -> do
    script <- sMat p $ Proxy @script
    pure $ PropScript root script

instance
  ( SMat p prop PropertyVL
  ) =>
  SMat p ('StaticPropRef @'TypeLevel prop)
         PropertyVL where
  sMat p _ = sMat p $ Proxy @prop

instance
  ( SMat p root (EssenceVL, PropertyRootVL)
  ) =>
  SMat p ('StaticProp @'TypeLevel root)
         PropertyVL where
  sMat p _ = withProperty p (Proxy @root) $ \root -> do
    pure $ StaticPropRef $ StaticProp root

-- Statically materialize property key value list

instance
  SMat p (SrcPropKVs '[]) ResPropKVs where
  sMat p _ = pure []

instance
  ( SMat p propKV PropertyKeyValueVL
  , SMat p (SrcPropKVs propKVs) ResPropKVs
  ) =>
  SMat p (SrcPropKVs (propKV ': propKVs)) ResPropKVs where
  sMat p _ = do
    propKV  <- sMat p $ Proxy @propKV
    propKVs <- sMat p $ Proxy @(SrcPropKVs propKVs)
    pure $ propKV : propKVs

-- Statically materialize property key value

instance
  ( SMat p ess EssenceVL
  , SMat p propOwn PropertyOwningVL
  ) =>
  SMat p ('PropKeyVal @'TypeLevel ess propOwn)
         PropertyKeyValueVL where
  sMat p _ = do
    ess     <- sMat p $ Proxy @ess
    propOwn <- sMat p $ Proxy @propOwn
    pure $ PropKeyVal ess propOwn

instance
  ( SMat p ess EssenceVL
  , SMat p (PropOwns propOwns) [PropertyOwningVL]
  ) =>
  SMat p ('PropKeyBag @'TypeLevel ess propOwns)
         PropertyKeyValueVL where
  sMat p _ = do
    ess      <- sMat p $ Proxy @ess
    propOwns <- sMat p $ Proxy @(PropOwns propOwns)
    pure $ PropKeyBag ess propOwns

-- Statically materialize property ownings

instance
  SMat p (PropOwns '[]) [PropertyOwningVL] where
  sMat p _ = pure []

instance
  ( SMat p propOwn PropertyOwningVL
  , SMat p (PropOwns propOwns) [PropertyOwningVL]
  ) =>
  SMat p (PropOwns (propOwn ': propOwns))
         [PropertyOwningVL] where
  sMat p _ = do
    propOwn  <- sMat p $ Proxy @propOwn
    propOwns <- sMat p $ Proxy @(PropOwns propOwns)
    pure $ propOwn : propOwns

-- Statically materialize property owning

instance
  ( SMat p prop PropertyVL
  ) =>
  SMat p ('OwnProp @'TypeLevel prop)
         PropertyOwningVL where
  sMat p _ = do
    prop <- sMat p $ Proxy @prop
    pure $ OwnProp prop

instance
  ( SMat p prop PropertyVL
  ) =>
  SMat p ('SharedProp @'TypeLevel prop)
         PropertyOwningVL where
  sMat p _ = do
    prop <- sMat p $ Proxy @prop
    pure $ SharedProp prop

-- Statically materialize list of properties

instance
  SMat p (Props '[]) [PropertyVL] where
  sMat p _ = pure []

instance
  ( SMat p prop PropertyVL
  , SMat p (Props props) [PropertyVL]
  ) =>
  SMat p (Props (prop ': props)) [PropertyVL] where
  sMat p _ = do
    prop  <- sMat p $ Proxy @prop
    props <- sMat p $ Proxy @(Props props)
    pure $ prop : props
