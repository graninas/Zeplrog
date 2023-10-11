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
  :: SMat p root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  => p
  -> Proxy root
  -> SMaterializer (Essence 'ValueLevel, Property 'ValueLevel)
  -> SMaterializer (Essence 'ValueLevel, Property 'ValueLevel)
withProperty p rootProxy matProp = do
  SEnv dbg propsRef <- ask
  props1   <- readTVarIO propsRef
  (ess, _) <- sMat p rootProxy

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
  ( SMat p ess (Essence 'ValueLevel)
  ) =>
  SMat p ('EssStaticRoot @'TypeLevel ess)
      (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel) where
  sMat p _ = do
    ess <- sMat p $ Proxy @ess
    pure (ess, EssStaticRoot ess)

instance
  ( SMat p ess (Essence 'ValueLevel)
  , SMat p prop (Essence 'ValueLevel, Property 'ValueLevel)
  ) =>
  SMat p ('PropStaticRoot @'TypeLevel ess prop)
      (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel) where
  sMat p _ = do
    (ess, prop) <- sMat p $ Proxy @prop
    pure (ess, PropStaticRoot ess prop)

-- Statically materialize property

instance
  ( SMat p root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  , SMat p (SrcPropKVs propKVs) ResPropKVs
  ) =>
  SMat p ('PropDict @'TypeLevel root propKVs)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  sMat p _ = withProperty p (Proxy @root) $ do
    (ess, root) <- sMat p $ Proxy @root
    propKVs <- sMat p $ Proxy @(SrcPropKVs propKVs)
    pure (ess, PropDict root propKVs)

instance
  ( SMat p val (ValDef 'ValueLevel)
  , SMat p root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  ) =>
  SMat p ('PropVal @'TypeLevel root val)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  sMat p _ = withProperty p (Proxy @root) $ do
    (ess, root) <- sMat p $ Proxy @root
    val <- sMat p $ Proxy @val
    pure (ess, PropVal root val)

instance
  ( SMat p val (ValDef 'ValueLevel)
  , SMat p root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  ) =>
  SMat p ('PropConst root val)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  sMat p _ = withProperty p (Proxy @root) $ do
    (ess, root) <- sMat p $ Proxy @root
    val  <- sMat p $ Proxy @val
    pure (ess, PropConst root val)

instance
  ( SMat p root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  , SMat p script (Script 'ValueLevel)
  ) =>
  SMat p ('PropScript @'TypeLevel root script)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  sMat p _ = withProperty p (Proxy @root) $ do
    (ess, root) <- sMat p $ Proxy @root
    script      <- sMat p $ Proxy @script
    pure (ess, PropScript root script)

instance
  ( SMat p prop (Essence 'ValueLevel, Property 'ValueLevel)
  ) =>
  SMat p ('StaticPropRef @'TypeLevel prop)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  sMat p _ = sMat p $ Proxy @prop

instance
  ( SMat p root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  ) =>
  SMat p ('StaticProp @'TypeLevel root)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  sMat p _ = withProperty p (Proxy @root) $ do
    (ess, root) <- sMat p $ Proxy @root
    pure (ess, StaticPropRef $ StaticProp root)

-- Statically materialize property key values

instance
  SMat p (SrcPropKVs '[]) ResPropKVs where
  sMat p _ = pure []

instance
  ( SMat p propKV (PropertyKeyValue 'ValueLevel)
  , SMat p (SrcPropKVs propKVs) ResPropKVs
  ) =>
  SMat p (SrcPropKVs (propKV ': propKVs)) ResPropKVs where
  sMat p _ = do
    propKV  <- sMat p $ Proxy @propKV
    propKVs <- sMat p $ Proxy @(SrcPropKVs propKVs)
    pure $ propKV : propKVs

-- Statically materialize Prop Key Val

instance
  ( SMat p ess (Essence 'ValueLevel)
  , SMat p propOwn (PropertyOwning 'ValueLevel)
  ) =>
  SMat p ('PropKeyVal @'TypeLevel ess propOwn)
      (PropertyKeyValue 'ValueLevel) where
  sMat p _ = do
    ess     <- sMat p $ Proxy @ess
    propOwn <- sMat p $ Proxy @propOwn
    pure $ PropKeyVal ess propOwn

instance
  ( SMat p ess (Essence 'ValueLevel)
  , SMat p (PropOwns propOwns) [PropertyOwning 'ValueLevel]
  ) =>
  SMat p ('PropKeyBag @'TypeLevel ess propOwns)
      (PropertyKeyValue 'ValueLevel) where
  sMat p _ = do
    ess      <- sMat p $ Proxy @ess
    propOwns <- sMat p $ Proxy @(PropOwns propOwns)
    pure $ PropKeyBag ess propOwns

-- Statically materialize property owning

instance
  SMat p (PropOwns '[]) [PropertyOwning 'ValueLevel] where
  sMat p _ = pure []

instance
  ( SMat p propOwn (PropertyOwning 'ValueLevel)
  , SMat p (PropOwns propOwns) [PropertyOwning 'ValueLevel]
  ) =>
  SMat p (PropOwns (propOwn ': propOwns))
      [PropertyOwning 'ValueLevel] where
  sMat p _ = do
    propOwn  <- sMat p $ Proxy @propOwn
    propOwns <- sMat p $ Proxy @(PropOwns propOwns)
    pure $ propOwn : propOwns

instance
  ( SMat p prop (Essence 'ValueLevel, Property 'ValueLevel)
  ) =>
  SMat p ('OwnProp @'TypeLevel prop)
      (PropertyOwning 'ValueLevel) where
  sMat p _ = do
    (_, prop) <- sMat p $ Proxy @prop
    pure $ OwnProp prop

instance
  ( SMat p prop (Essence 'ValueLevel, Property 'ValueLevel)
  ) =>
  SMat p ('SharedProp @'TypeLevel prop)
      (PropertyOwning 'ValueLevel) where
  sMat p _ = do
    (_, prop) <- sMat p $ Proxy @prop
    pure $ SharedProp prop
