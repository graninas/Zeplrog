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
  :: SMat () root (EssenceVL, PropertyRootVL)
  => ()
  -> Proxy root
  -> (PropertyRootVL -> SMaterializer PropertyVL)
  -> SMaterializer PropertyVL
withProperty () rootProxy matPropF = do
  SEnv dbg _ _ statEssencesVar <- ask

  (ess, root) <- sMat () rootProxy

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
  ( SMat () ess EssenceVL
  ) =>
  SMat () ('EssRoot @'TypeLevel ess)
         (EssenceVL, PropertyRootVL) where
  sMat () _ = do
    ess <- sMat () $ Proxy @ess
    pure (ess, EssRoot ess)

instance
  ( SMat () ess EssenceVL
  , SMat () prop (EssenceVL, PropertyVL)
  ) =>
  SMat () ('PropRoot @'TypeLevel ess prop)
         (EssenceVL, PropertyRootVL) where
  sMat () _ = do
    (ess, prop) <- sMat () $ Proxy @prop
    pure (ess, PropRoot ess prop)

-- Statically materialize property

instance
  ( SMat () (SrcPropKVs propKVs) ResPropKVs
  , SMat () root (EssenceVL, PropertyRootVL)
  ) =>
  SMat () ('PropDict @'TypeLevel root propKVs)
         PropertyVL where
  sMat () _ = withProperty () (Proxy @root) $ \root -> do
    propKVs <- sMat () $ Proxy @(SrcPropKVs propKVs)
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
  ( SMat () ess EssenceVL
  , SMat () abstractProp PropertyVL
  , SMat () (SrcPropKVs propKVs) ResPropKVs
  ) =>
  SMat () ('DerivedProp @'TypeLevel ess abstractProp propKVs)
         PropertyVL where
  sMat () _ = do
    ess <- sMat () $ Proxy @ess

    sTraceDebug $ "Deriving property: " <> show ess

    abstractProp <- sMat () $ Proxy @abstractProp

    case abstractProp of
      PropDict root abstractPropKVs -> do
        let abstractPropEss = getEssence root
        sTraceDebug $ "Abstract property to derive: " <> show abstractPropEss

        statPropId <- getNextStaticPropertyId

        propKVs :: [PropertyKeyValueVL] <- sMat () $ Proxy @(SrcPropKVs propKVs)
        let propKVs' = mergePropKVs propKVs abstractPropKVs

        let prop = PropDict (PropRoot ess abstractProp) propKVs'
        addStaticProperty (statPropId, ess, prop)
        sTraceDebug $ show ess <> ": new property is derived: " <> show statPropId

        pure prop

      _ -> error "abstract non-dict prop is not supported yet"

instance
  ( SMat () val ValDefVL
  , SMat () root (EssenceVL, PropertyRootVL)
  ) =>
  SMat () ('PropVal @'TypeLevel root val)
         PropertyVL where
  sMat () _ = withProperty () (Proxy @root) $ \root -> do
    val <- sMat () $ Proxy @val
    pure $ PropVal root val

instance
  ( SMat () script ScriptVL
  , SMat () root (EssenceVL, PropertyRootVL)
  ) =>
  SMat () ('PropScript @'TypeLevel root script)
         PropertyVL where
  sMat () _ = withProperty () (Proxy @root) $ \root -> do
    script <- sMat () $ Proxy @script
    pure $ PropScript root script

instance
  ( SMat () prop PropertyVL
  ) =>
  SMat () ('StaticPropRef @'TypeLevel prop)
         PropertyVL where
  sMat () _ = sMat () $ Proxy @prop

instance
  ( SMat () root (EssenceVL, PropertyRootVL)
  ) =>
  SMat () ('StaticProp @'TypeLevel root)
         PropertyVL where
  sMat () _ = withProperty () (Proxy @root) $ \root -> do
    pure $ StaticPropRef $ StaticProp root

-- Statically materialize property key value list

instance
  SMat () (SrcPropKVs '[]) ResPropKVs where
  sMat () _ = pure []

instance
  ( SMat () propKV PropertyKeyValueVL
  , SMat () (SrcPropKVs propKVs) ResPropKVs
  ) =>
  SMat () (SrcPropKVs (propKV ': propKVs)) ResPropKVs where
  sMat () _ = do
    propKV  <- sMat () $ Proxy @propKV
    propKVs <- sMat () $ Proxy @(SrcPropKVs propKVs)
    pure $ propKV : propKVs

-- Statically materialize property key value

instance
  ( SMat () ess EssenceVL
  , SMat () propOwn PropertyOwningVL
  ) =>
  SMat () ('PropKeyVal @'TypeLevel ess propOwn)
         PropertyKeyValueVL where
  sMat () _ = do
    ess     <- sMat () $ Proxy @ess
    propOwn <- sMat () $ Proxy @propOwn
    pure $ PropKeyVal ess propOwn

instance
  ( SMat () ess EssenceVL
  , SMat () (PropOwns propOwns) [PropertyOwningVL]
  ) =>
  SMat () ('PropKeyBag @'TypeLevel ess propOwns)
         PropertyKeyValueVL where
  sMat () _ = do
    ess      <- sMat () $ Proxy @ess
    propOwns <- sMat () $ Proxy @(PropOwns propOwns)
    pure $ PropKeyBag ess propOwns

-- Statically materialize property ownings

instance
  SMat () (PropOwns '[]) [PropertyOwningVL] where
  sMat () _ = pure []

instance
  ( SMat () propOwn PropertyOwningVL
  , SMat () (PropOwns propOwns) [PropertyOwningVL]
  ) =>
  SMat () (PropOwns (propOwn ': propOwns))
         [PropertyOwningVL] where
  sMat () _ = do
    propOwn  <- sMat () $ Proxy @propOwn
    propOwns <- sMat () $ Proxy @(PropOwns propOwns)
    pure $ propOwn : propOwns

-- Statically materialize property owning

instance
  ( SMat () prop PropertyVL
  ) =>
  SMat () ('OwnProp @'TypeLevel prop)
         PropertyOwningVL where
  sMat () _ = do
    prop <- sMat () $ Proxy @prop
    pure $ OwnProp prop

instance
  ( SMat () prop PropertyVL
  ) =>
  SMat () ('SharedProp @'TypeLevel prop)
         PropertyOwningVL where
  sMat () _ = do
    prop <- sMat () $ Proxy @prop
    pure $ SharedProp prop

-- Statically materialize list of properties

instance
  SMat () (Props '[]) [PropertyVL] where
  sMat () _ = pure []

instance
  ( SMat () prop PropertyVL
  , SMat () (Props props) [PropertyVL]
  ) =>
  SMat () (Props (prop ': props)) [PropertyVL] where
  sMat () _ = do
    prop  <- sMat () $ Proxy @prop
    props <- sMat () $ Proxy @(Props props)
    pure $ prop : props
