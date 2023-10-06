{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Materialization where

import ZP.Prelude

import ZP.Domain.Static.Model
import qualified ZP.Domain.Dynamic.Model as DMod

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map as Map

-- N.B. Good side of the typed model that when a new typed data comes,
-- the compiler will tell if all the Mat instances are available for it.
-- Enables a gradual development of the model.



---------- Interface ------------------

data Env = Env
  { sharedProps :: TVar (Map.Map DMod.Essence DMod.DynamicProperty)
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
  Mat ('Ess symb) DMod.Essence where
  mat _ _ = pure $ symbolVal (Proxy @symb)

instance
  Mat ess DMod.Essence =>
  Mat ('EssRoot ess) DMod.Essence where
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
  , Mat root DMod.Essence
  ) =>
  Mat ('PropVal root valDef) DMod.DynamicProperty where
  mat False _ = do
    ess        <- mat False $ Proxy @root
    val        <- mat False $ Proxy @valDef
    valVar     <- liftIO $ newTVarIO val
    dynValVar  <- liftIO $ newTVarIO $ Just $ DMod.VarValue valVar
    propsVar   <- liftIO $ newTVarIO Map.empty
    pure (DMod.DynamicProperty ess propsVar dynValVar)
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
  , Mat root DMod.Essence
  ) =>
  Mat ('PropConst root valDef) DMod.DynamicProperty where
  mat False _ = do
    ess        <- mat False $ Proxy @root
    val        <- mat False $ Proxy @valDef
    dynValVar  <- liftIO $ newTVarIO $ Just $ DMod.ConstValue val
    propsVar   <- liftIO $ newTVarIO Map.empty
    pure (DMod.DynamicProperty ess propsVar dynValVar)
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



-- Materialize owning/sharing

instance Mat prop DMod.DynamicProperty =>
  Mat ('OwnProp prop) DMod.DynamicPropertyOwning where
  mat _ _ = do
    prop <- mat False $ Proxy @prop
    pure $ DMod.OwnDynamicProperty prop

instance Mat prop DMod.DynamicProperty =>
  Mat ('SharedProp prop) DMod.DynamicPropertyOwning where
  mat _ _ = do
    prop <- mat True $ Proxy @prop
    pure $ DMod.SharedDynamicProperty prop
