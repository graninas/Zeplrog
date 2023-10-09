{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materializer

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map as Map


---------- Materialization --------------

data Essences essPath

data SrcPropKVs propKVs

data PropOwns propOwns

type ResPropKVs = [PropertyKeyValue 'ValueLevel]

withProperty
  :: Mat root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  => Proxy root
  -> Materializer (Essence 'ValueLevel, Property 'ValueLevel)
  -> Materializer (Essence 'ValueLevel, Property 'ValueLevel)
withProperty rootProxy matProp = do
  Env propsVar <- ask
  props <- readTVarIO propsVar

  (ess, root) <- mat rootProxy

  case Map.lookup ess props of
    Just prop -> pure (ess, prop)
    Nothing -> do
      (_, prop) <- matProp
      let props' = Map.insert ess prop props
      atomically $ writeTVar propsVar props'
      pure (ess, prop)

-- Materialize property root and essence

instance
  KnownSymbol symb =>
  Mat ('Ess @'TypeLevel symb) (Essence 'ValueLevel) where
  mat _ = pure $ Ess $ symbolVal (Proxy @symb)

instance
  Mat ess (Essence 'ValueLevel) =>
  Mat ('EssStaticRoot @'TypeLevel ess)
      (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel) where
  mat _ = do
    ess <- mat $ Proxy @ess
    pure (ess, EssStaticRoot ess)

-- Materialize static root

instance
  ( Mat ess (Essence 'ValueLevel)
  , Mat statProp (Essence 'ValueLevel, StaticProperty 'ValueLevel)
  ) =>
  Mat ('PropStaticRoot @'TypeLevel ess statProp)
      (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel) where
  mat _ = do
    ess <- mat $ Proxy @ess
    (_, statProp) <- mat $ Proxy @statProp
    pure (ess, PropStaticRoot ess statProp)

-- Materialize values

instance KnownNat intVal =>
  Mat ('IntValue @'TypeLevel intVal)
      (ValDef 'ValueLevel) where
  mat _ = pure
      $ IntValue
      $ fromIntegral
      $ natVal
      $ Proxy @intVal

instance
  ( Mat val1 (ValDef 'ValueLevel)
  , Mat val2 (ValDef 'ValueLevel)
  ) =>
  Mat ('PairValue @'TypeLevel val1 val2) (ValDef 'ValueLevel) where
  mat _ = do
    val1 <- mat $ Proxy @val1
    val2 <- mat $ Proxy @val2
    pure $ PairValue val1 val2

instance
  ( Mat (Essences essPath) [Essence 'ValueLevel]
  ) =>
  Mat ('PropRefValue @'TypeLevel essPath)
      (ValDef 'ValueLevel) where
  mat _ = do
    path <- mat $ Proxy @(Essences essPath)
    pure $ PropRefValue path

-- Materialize property

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
  , Mat (SrcPropKVs propKVs) ResPropKVs
  ) =>
  Mat ('PropDict @'TypeLevel root propKVs)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  mat _ = withProperty (Proxy @root) $ do
    (ess, root) <- mat $ Proxy @root
    propKVs <- mat $ Proxy @(SrcPropKVs propKVs)
    pure (ess, PropDict root propKVs)

instance
  ( Mat staticProp (Essence 'ValueLevel, StaticProperty 'ValueLevel)
  ) =>
  Mat ('StaticPropRef @'TypeLevel staticProp)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  mat _ = do
    (ess, sp) <- mat $ Proxy @staticProp
    pure (ess, StaticPropRef sp)

instance
  ( Mat root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  ) =>
  Mat ('PropScript @'TypeLevel root script)
      (Essence 'ValueLevel, Property 'ValueLevel) where
  mat _ = withProperty (Proxy @root) $ do
    (ess, root) <- mat $ Proxy @root
    pure (ess, PropScript root NoScript)    -- TODO: temporary

-- Materialize property key values

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

-- Materialize Essence path

instance
  Mat (Essences '[]) [Essence 'ValueLevel] where
  mat _ = pure []

instance
  ( Mat ess (Essence 'ValueLevel)
  , Mat (Essences essPath) [Essence 'ValueLevel]
  ) =>
  Mat (Essences (ess ': essPath))
      [Essence 'ValueLevel] where
  mat _ = do
    ess     <- mat $ Proxy @ess
    essPath <- mat $ Proxy @(Essences essPath)
    pure $ ess : essPath

-- Materialize static prop

instance
  ( Mat root (Essence 'ValueLevel, StaticPropertyRoot 'ValueLevel)
  ) =>
  Mat ('StaticProp @'TypeLevel root)
      (Essence 'ValueLevel, StaticProperty 'ValueLevel) where
  mat _ = do
    (ess, root) <- mat $ Proxy @root
    pure (ess, StaticProp root)

-- Materialize Prop Key Val

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

-- Materialize property owning

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
  Mat prop (Essence 'ValueLevel, Property 'ValueLevel) =>
  Mat ('OwnProp @'TypeLevel prop) (PropertyOwning 'ValueLevel) where
  mat _ = do
    (_, prop) <- mat $ Proxy @prop
    pure $ OwnProp prop

instance
  Mat prop (Essence 'ValueLevel, Property 'ValueLevel) =>
  Mat ('SharedProp @'TypeLevel prop) (PropertyOwning 'ValueLevel) where
  mat _ = do
    (_, prop) <- mat $ Proxy @prop
    pure $ SharedProp prop
