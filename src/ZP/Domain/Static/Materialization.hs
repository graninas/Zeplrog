{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization where

import ZP.Prelude

import ZP.Domain.Static.Model

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map as Map


---------- Interface ------------------

data Env = Env
  { sharedProps :: TVar
      (Map.Map (Essence 'ValueLevel) (Property 'ValueLevel))
  }

type Materializer a = ReaderT Env IO a

-- | Materialization type class.
class Mat a b | a -> b where
  mat :: Proxy a -> Materializer b

runMaterializer :: Materializer a -> IO (Env, a)
runMaterializer m = do
  sharedProps' <- liftIO $ newTVarIO Map.empty
  let env = Env sharedProps'
  res <- runReaderT m env
  pure (env, res)

mat' :: Mat a b => Proxy a -> IO b
mat' proxy = do
  (_, a) <- runMaterializer (mat proxy)
  pure a

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
  Mat ('PropScript @'TypeLevel script) (Property 'ValueLevel) where
  mat _ = error "script mat not implemented"

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
  ( Mat category (Category 'ValueLevel)
  , Mat propOwn (PropertyOwning 'ValueLevel)
  ) =>
  Mat ('PropKeyVal @'TypeLevel category propOwn)
      (PropertyKeyValue 'ValueLevel) where
  mat _ = do
    category <- mat $ Proxy @category
    propOwn  <- mat $ Proxy @propOwn
    pure $ PropKeyVal category propOwn


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
  ( Mat category (Category 'ValueLevel)
  , Mat (PropOwns propOwns) [PropertyOwning 'ValueLevel]
  ) =>
  Mat ('PropKeyBag @'TypeLevel category propOwns)
      (PropertyKeyValue 'ValueLevel) where
  mat _ = do
    category <- mat $ Proxy @category
    propOwns <- mat $ Proxy @(PropOwns propOwns)
    pure $ PropKeyBag category propOwns

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
