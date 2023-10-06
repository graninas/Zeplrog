{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Materialization where

import ZP.Prelude

import ZP.Domain.Static.Model
import qualified ZP.Domain.Dynamic.Model.Common as DMod
import qualified ZP.Domain.Dynamic.Model.Property as DMod
import qualified ZP.Domain.Browser.Language as Browser
import qualified ZP.Domain.Browser.Methods as Browser

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map as Map

-- N.B. Good side of the typed model that when a new typed data comes,
-- the compiler will tell if all the Mat instances are available for it.
-- Enables a gradual development of the model.



---------- Interface ------------------

data Env = Env
  { sharedProps :: TVar (Map.Map DMod.DynEssence DMod.DynamicProperty)
  }

type Materializer a = ReaderT Env IO a

type Shared = Bool

-- | Materialization type class.
class Mat a b | a -> b where
  mat :: Shared -> Proxy a -> Materializer b

runMaterializer :: Materializer a -> IO (Env, a)
runMaterializer m = do
  sharedProps' <- liftIO $ newTVarIO Map.empty
  let env = Env sharedProps'
  res <- runReaderT m env
  pure (env, res)

mat' :: Mat a b => Proxy a -> IO b
mat' proxy = do
  (_, a) <- runMaterializer (mat False proxy)
  pure a

---------- Materialization --------------

-- Materialize essence

instance
  KnownSymbol symb =>
  Mat ('Ess symb) DMod.DynEssence where
  mat _ _ = pure $ symbolVal (Proxy @symb)

instance
  Mat ess DMod.DynEssence =>
  Mat ('EssRoot ess) DMod.DynEssence where
  mat _ _ = mat False (Proxy @ess)

-- Materialize values

instance KnownNat intVal =>
  Mat ('IntValDef intVal) DMod.Value where
  mat _ _ = pure
      $ DMod.IntValue
      $ fromIntegral
      $ natVal
      $ Proxy @intVal

instance (Mat val1 DMod.Value, Mat val2 DMod.Value) =>
  Mat ('PairValDef val1 val2) DMod.Value where
  mat _ _ = do
    val1 <- mat False $ Proxy @val1
    val2 <- mat False $ Proxy @val2
    pure $ DMod.PairValue val1 val2

-- Materialize property

instance
  ( Mat valDef DMod.Value
  , Mat root DMod.DynEssence
  , Browser.Browse Browser.GetEssence ('PropVal root valDef) DMod.DynEssence
  ) =>
  Mat ('PropVal root valDef) DMod.DynamicProperty where
  mat False _ = do
    ess        <- mat False $ Proxy @root
    val        <- mat False $ Proxy @valDef
    valVar     <- liftIO $ newTVarIO val
    dynValVar  <- liftIO $ newTVarIO $ Just $ DMod.VarValue valVar
    propsVar   <- liftIO $ newTVarIO Map.empty
    let staticProp = Proxy @('PropVal root valDef)
    let staticPropRef = DMod.StaticPropRef staticProp
    pure (DMod.DynamicProperty ess staticPropRef propsVar dynValVar)
  mat True proxy = do
    Env spsVar <- ask
    sps <- liftIO $ readTVarIO spsVar
    ess <- mat False $ Proxy @root
    case Map.lookup ess sps of
      Just prop -> pure prop
      Nothing   -> do
        prop <- mat False proxy
        let sps' = Map.insert ess prop sps
        liftIO $ atomically $ writeTVar spsVar sps'
        pure prop


instance
  ( Mat valDef DMod.Value
  , Mat root DMod.DynEssence
  , Browser.Browse Browser.GetEssence ('PropConst root valDef) DMod.DynEssence
  ) =>
  Mat ('PropConst root valDef) DMod.DynamicProperty where
  mat False _ = do
    ess        <- mat False $ Proxy @root
    val        <- mat False $ Proxy @valDef
    dynValVar  <- liftIO $ newTVarIO $ Just $ DMod.ConstValue val
    propsVar   <- liftIO $ newTVarIO Map.empty
    let staticProp = Proxy @('PropConst root valDef)
    let staticPropRef = DMod.StaticPropRef staticProp
    pure (DMod.DynamicProperty ess staticPropRef propsVar dynValVar)
  mat True proxy = do
    Env spsVar <- ask
    sps <- liftIO $ readTVarIO spsVar
    ess <- mat False $ Proxy @root
    case Map.lookup ess sps of
      Just prop -> pure prop
      Nothing   -> do
        prop <- mat False proxy
        let sps' = Map.insert ess prop sps
        liftIO $ atomically $ writeTVar spsVar sps'
        pure prop


data PropKVs propKVs

type DynProps = [(DMod.DynEssence, DMod.DynamicProperty)]

instance
  Mat (PropKVs '[]) DynProps where
  mat _ _ = pure []

instance
  ( Mat propKV (DMod.DynEssence, [DMod.DynamicPropertyOwning])
  , Mat (PropKVs propKVs) DynProps
  ) =>
  Mat (PropKVs (propKV ': propKVs)) DynProps where
  mat _ _ = do
    (ess, propOwns) <- mat False $ Proxy @propKV
    propKVs         <- mat False $ Proxy @(PropKVs propKVs)
    error "Mat PropKVs not implemented"

instance
  ( Mat root DMod.DynEssence
  , Mat (PropKVs propKVs) DynProps
  ) =>
  Mat ('PropDict root propKVs) DMod.DynamicProperty where
  mat False _ = do
    dynProps <- mat False $ Proxy @(PropKVs propKVs)
    error "1"
    -- ess        <- mat False $ Proxy @root
    -- val        <- mat False $ Proxy @valDef
    -- dynValVar  <- liftIO $ newTVarIO $ Just $ DMod.ConstValue val
    -- propsVar   <- liftIO $ newTVarIO Map.empty
    -- let staticProp = Proxy @('PropConst root valDef)
    -- let staticPropRef = DMod.StaticPropRef staticProp
    -- pure (DMod.DynamicProperty ess staticPropRef propsVar dynValVar)
  mat True proxy = do
    error "mat True for PropDict not implemented"
    -- Env spsVar <- ask
    -- sps <- liftIO $ readTVarIO spsVar
    -- ess <- mat False $ Proxy @root
    -- case Map.lookup ess sps of
    --   Just prop -> pure prop
    --   Nothing   -> do
    --     prop <- mat False proxy
    --     let sps' = Map.insert ess prop sps
    --     liftIO $ atomically $ writeTVar spsVar sps'
    --     pure prop



instance
  Mat ('PropRef essPath) DMod.DynamicProperty where
  mat False _ = do
    error "PropRef False not implemented"
  mat True proxy = do
    error "PropRef True not implemented"

instance
  Mat ('StaticPropRef staticProp) DMod.DynamicProperty where
  mat False _ = do
    error "StaticPropRef False not implemented"
  mat True proxy = do
    error "StaticPropRef True not implemented"

instance
  Mat ('PropScript script) DMod.DynamicProperty where
  mat False _ = do
    error "PropScript False not implemented"
  mat True proxy = do
    error "PropScript True not implemented"


-- Materialize Prop Key Val

instance
  ( Mat category DMod.DynEssence
  , Mat propOwn DMod.DynamicPropertyOwning
  ) =>
  Mat ('PropKeyVal category propOwn)
      (DMod.DynEssence, [DMod.DynamicPropertyOwning]) where
  mat _ _ = do
    ess     <- mat False $ Proxy @category
    propOwn <- mat False $ Proxy @propOwn
    pure (ess, [propOwn])


data PropOwns propOwns

instance
  Mat (PropOwns '[]) [DMod.DynamicPropertyOwning] where
  mat _ _ = pure []

instance
  ( Mat propOwn DMod.DynamicPropertyOwning
  , Mat (PropOwns propOwns) [DMod.DynamicPropertyOwning]
  ) =>
  Mat (PropOwns (propOwn ': propOwns))
      [DMod.DynamicPropertyOwning] where
  mat _ _ = do
    propOwn  <- mat False $ Proxy @propOwn
    propOwns <- mat False $ Proxy @(PropOwns propOwns)
    pure $ propOwn : propOwns

instance
  ( Mat category DMod.DynEssence
  , Mat (PropOwns propOwns) [DMod.DynamicPropertyOwning]
  ) =>
  Mat ('PropKeyBag category propOwns)
      (DMod.DynEssence, [DMod.DynamicPropertyOwning]) where
  mat _ _ = do
    ess      <- mat False $ Proxy @category
    propOwns <- mat False $ Proxy @(PropOwns propOwns)
    pure (ess, propOwns)

-- Materialize owning/sharing

instance
  Mat prop DMod.DynamicProperty =>
  Mat ('OwnProp prop) DMod.DynamicPropertyOwning where
  mat _ _ = do
    prop <- mat False $ Proxy @prop
    pure $ DMod.OwnDynamicProperty prop

instance
  Mat prop DMod.DynamicProperty =>
  Mat ('SharedProp prop) DMod.DynamicPropertyOwning where
  mat _ _ = do
    prop <- mat True $ Proxy @prop
    pure $ DMod.SharedDynamicProperty prop
