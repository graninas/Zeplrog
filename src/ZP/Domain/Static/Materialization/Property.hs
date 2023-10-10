{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Property where

import ZP.Prelude

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
  :: Mat root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  => Proxy root
  -> Materializer (Essence 'ValueLevel, Property 'ValueLevel)
  -> Materializer (Essence 'ValueLevel, Property 'ValueLevel)
withProperty rootProxy matProp = do
  Env dbg propsRef <- ask
  props1 <- readIORef propsRef
  (ess, _) <- mat rootProxy

  when (DebugEnabled == dbg)
    $ trace ("Cur stat prop: " <> show ess)
    $ pure ()

  case Map.lookup ess props1 of
    Just prop -> pure (ess, prop)
    Nothing -> do
      (_, prop) <- matProp

      props2 <- readIORef propsRef
      let props2' = Map.insert ess prop props2
      writeIORef propsRef props2'

      pure (ess, prop)

-- Statically materialize property root

instance
  ( Mat ess (Essence 'ValueLevel)
  ) =>
  Mat ('EssStaticRoot @'TypeLevel ess)
      (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel) where
  mat _ = do
    ess <- mat $ Proxy @ess
    pure (ess, EssStaticRoot ess)

instance
  ( Mat ess (Essence 'ValueLevel)
  , Mat prop (Essence 'ValueLevel, Property 'ValueLevel)
  ) =>
  Mat ('PropStaticRoot @'TypeLevel ess prop)
      (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel) where
  mat _ = do
    (ess, prop) <- mat $ Proxy @prop
    pure (ess, PropStaticRoot ess prop)

-- Statically materialize property

instance
  ( Mat root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  , Mat (SrcPropKVs propKVs) ResPropKVs
  ) =>
  Mat ('PropDict @'TypeLevel root propKVs)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  mat _ = withProperty (Proxy @root) $ do
    (ess, root) <- mat $ Proxy @root
    propKVs <- mat $ Proxy @(SrcPropKVs propKVs)
    pure (ess, PropDict root propKVs)

instance
  ( Mat val (ValDef 'ValueLevel)
  , Mat root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  ) =>
  Mat ('PropVal @'TypeLevel root val)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  mat _ = withProperty (Proxy @root) $ do
    (ess, root) <- mat $ Proxy @root
    val <- mat $ Proxy @val
    pure (ess, PropVal root val)

instance
  ( Mat val (ValDef 'ValueLevel)
  , Mat root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  ) =>
  Mat ('PropConst root val)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  mat _ = withProperty (Proxy @root) $ do
    (ess, root) <- mat $ Proxy @root
    val  <- mat $ Proxy @val
    pure (ess, PropConst root val)

instance
  ( Mat root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  , Mat script (Script 'ValueLevel)
  ) =>
  Mat ('PropScript @'TypeLevel root script)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  mat _ = withProperty (Proxy @root) $ do
    (ess, root) <- mat $ Proxy @root
    script      <- mat $ Proxy @script
    pure (ess, PropScript root script)

instance
  ( Mat prop (Essence 'ValueLevel, Property 'ValueLevel)
  ) =>
  Mat ('StaticPropRef @'TypeLevel prop)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  mat _ = mat $ Proxy @prop

instance
  ( Mat root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  ) =>
  Mat ('StaticProp @'TypeLevel root)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  mat _ = withProperty (Proxy @root) $ do
    (ess, root) <- mat $ Proxy @root
    pure (ess, StaticPropRef $ StaticProp root)

-- Statically materialize property key values

instance
  Mat (SrcPropKVs '[]) ResPropKVs where
  mat _ = pure []

instance
  ( Mat propKV (PropertyKeyValue 'ValueLevel)
  , Mat (SrcPropKVs propKVs) ResPropKVs
  ) =>
  Mat (SrcPropKVs (propKV ': propKVs)) ResPropKVs where
  mat _ = do
    propKV  <- mat $ Proxy @propKV
    propKVs <- mat $ Proxy @(SrcPropKVs propKVs)
    pure $ propKV : propKVs

-- Statically materialize Prop Key Val

instance
  ( Mat ess (Essence 'ValueLevel)
  , Mat propOwn (PropertyOwning 'ValueLevel)
  ) =>
  Mat ('PropKeyVal @'TypeLevel ess propOwn)
      (PropertyKeyValue 'ValueLevel) where
  mat _ = do
    ess     <- mat $ Proxy @ess
    propOwn <- mat $ Proxy @propOwn
    pure $ PropKeyVal ess propOwn

instance
  ( Mat ess (Essence 'ValueLevel)
  , Mat (PropOwns propOwns) [PropertyOwning 'ValueLevel]
  ) =>
  Mat ('PropKeyBag @'TypeLevel ess propOwns)
      (PropertyKeyValue 'ValueLevel) where
  mat _ = do
    ess      <- mat $ Proxy @ess
    propOwns <- mat $ Proxy @(PropOwns propOwns)
    pure $ PropKeyBag ess propOwns

-- Statically materialize property owning

instance
  Mat (PropOwns '[]) [PropertyOwning 'ValueLevel] where
  mat _ = pure []

instance
  ( Mat propOwn (PropertyOwning 'ValueLevel)
  , Mat (PropOwns propOwns) [PropertyOwning 'ValueLevel]
  ) =>
  Mat (PropOwns (propOwn ': propOwns))
      [PropertyOwning 'ValueLevel] where
  mat _ = do
    propOwn  <- mat $ Proxy @propOwn
    propOwns <- mat $ Proxy @(PropOwns propOwns)
    pure $ propOwn : propOwns

instance
  ( Mat prop (Essence 'ValueLevel, Property 'ValueLevel)
  ) =>
  Mat ('OwnProp @'TypeLevel prop)
      (PropertyOwning 'ValueLevel) where
  mat _ = do
    (_, prop) <- mat $ Proxy @prop
    pure $ OwnProp prop

instance
  ( Mat prop (Essence 'ValueLevel, Property 'ValueLevel)
  ) =>
  Mat ('SharedProp @'TypeLevel prop)
      (PropertyOwning 'ValueLevel) where
  mat _ = do
    (_, prop) <- mat $ Proxy @prop
    pure $ SharedProp prop
