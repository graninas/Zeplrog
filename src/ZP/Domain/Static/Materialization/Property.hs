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


---------- Materialization --------------

data SrcPropKVs propKVs

data PropOwns propOwns

type ResPropKVs = [PropertyKeyValue 'ValueLevel]

withProperty
  :: SMat root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  => Proxy root
  -> SMaterializer (Essence 'ValueLevel, Property 'ValueLevel)
  -> SMaterializer (Essence 'ValueLevel, Property 'ValueLevel)
withProperty rootProxy matProp = do
  SEnv dbg propsRef <- ask
  props1   <- readTVarIO propsRef
  (ess, _) <- sMat rootProxy

  when (DebugEnabled == dbg)
    $ trace ("Cur stat prop: " <> show ess)
    $ pure ()

  case Map.lookup ess props1 of
    Just prop -> pure (ess, prop)
    Nothing -> do
      (_, prop) <- matProp

      props2 <- readTVarIO propsRef
      let props2' = Map.insert ess prop props2
      atomically $ writeTVar propsRef props2'

      pure (ess, prop)

-- Statically materialize property root

instance
  ( SMat ess (Essence 'ValueLevel)
  ) =>
  SMat ('EssStaticRoot @'TypeLevel ess)
      (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel) where
  sMat _ = do
    ess <- sMat $ Proxy @ess
    pure (ess, EssStaticRoot ess)

instance
  ( SMat ess (Essence 'ValueLevel)
  , SMat prop (Essence 'ValueLevel, Property 'ValueLevel)
  ) =>
  SMat ('PropStaticRoot @'TypeLevel ess prop)
      (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel) where
  sMat _ = do
    (ess, prop) <- sMat $ Proxy @prop
    pure (ess, PropStaticRoot ess prop)

-- Statically materialize property

instance
  ( SMat root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  , SMat (SrcPropKVs propKVs) ResPropKVs
  ) =>
  SMat ('PropDict @'TypeLevel root propKVs)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  sMat _ = withProperty (Proxy @root) $ do
    (ess, root) <- sMat $ Proxy @root
    propKVs <- sMat $ Proxy @(SrcPropKVs propKVs)
    pure (ess, PropDict root propKVs)

instance
  ( SMat val (ValDef 'ValueLevel)
  , SMat root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  ) =>
  SMat ('PropVal @'TypeLevel root val)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  sMat _ = withProperty (Proxy @root) $ do
    (ess, root) <- sMat $ Proxy @root
    val <- sMat $ Proxy @val
    pure (ess, PropVal root val)

instance
  ( SMat val (ValDef 'ValueLevel)
  , SMat root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  ) =>
  SMat ('PropConst root val)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  sMat _ = withProperty (Proxy @root) $ do
    (ess, root) <- sMat $ Proxy @root
    val  <- sMat $ Proxy @val
    pure (ess, PropConst root val)

instance
  ( SMat root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  , SMat script (Script 'ValueLevel)
  ) =>
  SMat ('PropScript @'TypeLevel root script)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  sMat _ = withProperty (Proxy @root) $ do
    (ess, root) <- sMat $ Proxy @root
    script      <- sMat $ Proxy @script
    pure (ess, PropScript root script)

instance
  ( SMat prop (Essence 'ValueLevel, Property 'ValueLevel)
  ) =>
  SMat ('StaticPropRef @'TypeLevel prop)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  sMat _ = sMat $ Proxy @prop

instance
  ( SMat root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  ) =>
  SMat ('StaticProp @'TypeLevel root)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  sMat _ = withProperty (Proxy @root) $ do
    (ess, root) <- sMat $ Proxy @root
    pure (ess, StaticPropRef $ StaticProp root)

-- Statically materialize property key values

instance
  SMat (SrcPropKVs '[]) ResPropKVs where
  sMat _ = pure []

instance
  ( SMat propKV (PropertyKeyValue 'ValueLevel)
  , SMat (SrcPropKVs propKVs) ResPropKVs
  ) =>
  SMat (SrcPropKVs (propKV ': propKVs)) ResPropKVs where
  sMat _ = do
    propKV  <- sMat $ Proxy @propKV
    propKVs <- sMat $ Proxy @(SrcPropKVs propKVs)
    pure $ propKV : propKVs

-- Statically materialize Prop Key Val

instance
  ( SMat ess (Essence 'ValueLevel)
  , SMat propOwn (PropertyOwning 'ValueLevel)
  ) =>
  SMat ('PropKeyVal @'TypeLevel ess propOwn)
      (PropertyKeyValue 'ValueLevel) where
  sMat _ = do
    ess     <- sMat $ Proxy @ess
    propOwn <- sMat $ Proxy @propOwn
    pure $ PropKeyVal ess propOwn

instance
  ( SMat ess (Essence 'ValueLevel)
  , SMat (PropOwns propOwns) [PropertyOwning 'ValueLevel]
  ) =>
  SMat ('PropKeyBag @'TypeLevel ess propOwns)
      (PropertyKeyValue 'ValueLevel) where
  sMat _ = do
    ess      <- sMat $ Proxy @ess
    propOwns <- sMat $ Proxy @(PropOwns propOwns)
    pure $ PropKeyBag ess propOwns

-- Statically materialize property owning

instance
  SMat (PropOwns '[]) [PropertyOwning 'ValueLevel] where
  sMat _ = pure []

instance
  ( SMat propOwn (PropertyOwning 'ValueLevel)
  , SMat (PropOwns propOwns) [PropertyOwning 'ValueLevel]
  ) =>
  SMat (PropOwns (propOwn ': propOwns))
      [PropertyOwning 'ValueLevel] where
  sMat _ = do
    propOwn  <- sMat $ Proxy @propOwn
    propOwns <- sMat $ Proxy @(PropOwns propOwns)
    pure $ propOwn : propOwns

instance
  ( SMat prop (Essence 'ValueLevel, Property 'ValueLevel)
  ) =>
  SMat ('OwnProp @'TypeLevel prop)
      (PropertyOwning 'ValueLevel) where
  sMat _ = do
    (_, prop) <- sMat $ Proxy @prop
    pure $ OwnProp prop

instance
  ( SMat prop (Essence 'ValueLevel, Property 'ValueLevel)
  ) =>
  SMat ('SharedProp @'TypeLevel prop)
      (PropertyOwning 'ValueLevel) where
  sMat _ = do
    (_, prop) <- sMat $ Proxy @prop
    pure $ SharedProp prop
