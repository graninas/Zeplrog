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

-- Materialize property root and essence

instance
  KnownSymbol symb =>
  Mat ('Ess @'TypeLevel symb) (Essence 'ValueLevel) where
  mat _ = pure $ Ess $ symbolVal (Proxy @symb)

instance
  Mat ess (Essence 'ValueLevel) =>
  Mat ('EssRoot @'TypeLevel ess) (PropertyRoot 'ValueLevel) where
  mat _ = EssRoot <$> (mat $ Proxy @ess)

instance
  ( Mat ess (Essence 'ValueLevel)
  , Mat prop (Property 'ValueLevel)
  ) =>
  Mat ('PropRoot @'TypeLevel ess prop) (PropertyRoot 'ValueLevel) where
  mat _ = do
    ess  <- mat $ Proxy @ess
    prop <- mat $ Proxy @prop
    pure $ PropRoot ess prop

-- Materialize static root

instance
  ( Mat ess (Essence 'ValueLevel)
  , Mat statProp (StaticProperty 'ValueLevel)
  ) =>
  Mat ('PropStaticRoot @'TypeLevel ess statProp)
      (StaticPropertyRoot 'ValueLevel) where
  mat _ = do
    ess      <- mat $ Proxy @ess
    statProp <- mat $ Proxy @statProp
    pure $ PropStaticRoot ess statProp

instance
  ( Mat ess (Essence 'ValueLevel)
  ) =>
  Mat ('EssStaticRoot @'TypeLevel ess)
      (StaticPropertyRoot 'ValueLevel) where
  mat _ = do
    ess  <- mat $ Proxy @ess
    pure $ EssStaticRoot ess

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

-- Materialize property

instance
  ( Mat val (ValDef 'ValueLevel)
  , Mat root (PropertyRoot 'ValueLevel)
  ) =>
  Mat ('PropVal @'TypeLevel root val)
      (Property 'ValueLevel) where
  mat _ = do
    root <- mat $ Proxy @root
    val  <- mat $ Proxy @val
    pure (PropVal root val)

instance
  ( Mat val (ValDef 'ValueLevel)
  , Mat root (PropertyRoot 'ValueLevel)
  ) =>
  Mat ('PropConst root val) (Property 'ValueLevel) where
  mat _ = do
    root <- mat $ Proxy @root
    val  <- mat $ Proxy @val
    pure $ PropConst root val

data SrcPropKVs propKVs

type ResPropKVs = [PropertyKeyValue 'ValueLevel]

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

instance
  ( Mat root (PropertyRoot 'ValueLevel)
  , Mat (SrcPropKVs propKVs) ResPropKVs
  ) =>
  Mat ('PropDict @'TypeLevel root propKVs)
      (Property 'ValueLevel) where
  mat _ = do
    root    <- mat $ Proxy @root
    propKVs <- mat $ Proxy @(SrcPropKVs propKVs)
    pure $ PropDict root propKVs

data Essences essPath

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

instance
  Mat (Essences essPath) [Essence 'ValueLevel] =>
  Mat ('PropRef @'TypeLevel essPath)
      (Property 'ValueLevel) where
  mat _ = do
    essPath <- mat $ Proxy @(Essences essPath)
    pure $ PropRef essPath

instance
  ( Mat staticProp (StaticProperty 'ValueLevel)
  ) =>
  Mat ('StaticPropRef @'TypeLevel staticProp)
      (Property 'ValueLevel) where
  mat _ = do
    sp <- mat $ Proxy @staticProp
    pure $ StaticPropRef sp

instance
  ( Mat ess (Essence 'ValueLevel)
  ) =>
  Mat ('PropScript @'TypeLevel ess script)
      (Property 'ValueLevel) where
  mat _ = do
    ess <- mat $ Proxy @ess
    pure $ PropScript ess NoScript    -- TODO: temporary

-- Materialize static prop

instance
  Mat root (StaticPropertyRoot 'ValueLevel) =>
  Mat ('StaticProp @'TypeLevel root)
      (StaticProperty 'ValueLevel) where
  mat _ = do
    root <- mat $ Proxy @root
    pure $ StaticProp root


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


data PropOwns propOwns

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
  ( Mat ess (Essence 'ValueLevel)
  , Mat (PropOwns propOwns) [PropertyOwning 'ValueLevel]
  ) =>
  Mat ('PropKeyBag @'TypeLevel ess propOwns)
      (PropertyKeyValue 'ValueLevel) where
  mat _ = do
    ess      <- mat $ Proxy @ess
    propOwns <- mat $ Proxy @(PropOwns propOwns)
    pure $ PropKeyBag ess propOwns

-- Materialize owning/sharing

instance
  Mat prop (Property 'ValueLevel) =>
  Mat ('OwnProp @'TypeLevel prop) (PropertyOwning 'ValueLevel) where
  mat _ = do
    prop <- mat $ Proxy @prop
    pure $ OwnProp prop

instance
  Mat prop (Property 'ValueLevel) =>
  Mat ('SharedProp @'TypeLevel prop) (PropertyOwning 'ValueLevel) where
  mat _ = do
    prop <- mat $ Proxy @prop
    pure $ SharedProp prop
