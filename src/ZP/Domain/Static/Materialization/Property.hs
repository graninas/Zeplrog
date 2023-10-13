{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Property where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Static.Model
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

withProperty
  :: SMat p root (EssenceVL, PropertyRootVL)
  => p
  -> Proxy root
  -> SMaterializer (EssenceVL, PropertyVL)
  -> SMaterializer (EssenceVL, PropertyVL)
withProperty p rootProxy matProp = do
  SEnv dbg statPropIdVar statPropsVar statEssencesVar <- ask

  (ess, _) <- sMat p rootProxy

  when (dbg == DebugEnabled)
    $ trace ("Static property to introduce: " <> show ess)
    $ pure ()

  esss <- readTVarIO statEssencesVar

  (msg, prop) <- case Map.lookup ess esss of
      Just (statPropId, prop) ->
        pure (show ess <> ": already exists: " <> show statPropId, (ess, prop))
      Nothing -> do
        p@(ess, prop) <- matProp

        statPropId <- addStaticProperty p

        pure (show ess <> ": created: " <> show statPropId, p)

  when (dbg == DebugEnabled)
    $ trace msg
    $ pure ()

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
  ( SMat p root (EssenceVL, PropertyRootVL)
  , SMat p (SrcPropKVs propKVs) ResPropKVs
  ) =>
  SMat p ('PropDict @'TypeLevel root propKVs)
         (EssenceVL, PropertyVL) where
  sMat p _ = withProperty p (Proxy @root) $ do
    (ess, root) <- sMat p $ Proxy @root
    propKVs <- sMat p $ Proxy @(SrcPropKVs propKVs)
    pure (ess, PropDict root propKVs)

instance
  ( SMat p val ValDefVL
  , SMat p root (EssenceVL, PropertyRootVL)
  ) =>
  SMat p ('PropVal @'TypeLevel root val)
         (EssenceVL, PropertyVL) where
  sMat p _ = withProperty p (Proxy @root) $ do
    (ess, root) <- sMat p $ Proxy @root
    val <- sMat p $ Proxy @val
    pure (ess, PropVal root val)

instance
  ( SMat p root (EssenceVL, PropertyRootVL)
  , SMat p script (Script 'ValueLevel)
  ) =>
  SMat p ('PropScript @'TypeLevel root script)
         (EssenceVL, PropertyVL) where
  sMat p _ = withProperty p (Proxy @root) $ do
    (ess, root) <- sMat p $ Proxy @root
    script      <- sMat p $ Proxy @script
    pure (ess, PropScript root script)

instance
  ( SMat p prop (EssenceVL, PropertyVL)
  ) =>
  SMat p ('StaticPropRef @'TypeLevel prop)
         (EssenceVL, PropertyVL) where
  sMat p _ = sMat p $ Proxy @prop

instance
  ( SMat p root (EssenceVL, PropertyRootVL)
  ) =>
  SMat p ('StaticProp @'TypeLevel root)
         (EssenceVL, PropertyVL) where
  sMat p _ = withProperty p (Proxy @root) $ do
    (ess, root) <- sMat p $ Proxy @root
    pure (ess, StaticPropRef $ StaticProp root)

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
         (PropertyKeyValueVL) where
  sMat p _ = do
    ess     <- sMat p $ Proxy @ess
    propOwn <- sMat p $ Proxy @propOwn
    pure $ PropKeyVal ess propOwn

instance
  ( SMat p ess EssenceVL
  , SMat p (PropOwns propOwns) [PropertyOwningVL]
  ) =>
  SMat p ('PropKeyBag @'TypeLevel ess propOwns)
         (PropertyKeyValueVL) where
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
  ( SMat p prop (EssenceVL, PropertyVL)
  ) =>
  SMat p ('OwnProp @'TypeLevel prop)
         (PropertyOwningVL) where
  sMat p _ = do
    (_, prop) <- sMat p $ Proxy @prop
    pure $ OwnProp prop

instance
  ( SMat p prop (EssenceVL, PropertyVL)
  ) =>
  SMat p ('SharedProp @'TypeLevel prop)
         (PropertyOwningVL) where
  sMat p _ = do
    (_, prop) <- sMat p $ Proxy @prop
    pure $ SharedProp prop

-- Statically materialize list of properties

instance
  SMat p (Props '[]) [(EssenceVL, PropertyVL)] where
  sMat p _ = pure []

instance
  ( SMat p prop (EssenceVL, PropertyVL)
  , SMat p (Props props) [(EssenceVL, PropertyVL)]
  ) =>
  SMat p (Props (prop ': props)) [(EssenceVL, PropertyVL)] where
  sMat p _ = do
    prop  <- sMat p $ Proxy @prop
    props <- sMat p $ Proxy @(Props props)
    pure $ prop : props
