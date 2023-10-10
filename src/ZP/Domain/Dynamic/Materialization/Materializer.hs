{-# LANGUAGE DataKinds #-}
module ZP.Domain.Dynamic.Materialization.Materializer where

import ZP.Prelude

import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Dynamic.Model

import qualified Data.Map.Strict as Map


---------- Dynamic materializer interface ------------------

data Env = Env
  SMat.StaticProperties
  (TVar (Map.Map Essence Property))

type Materializer a = ReaderT Env IO a

type Shared = Bool

-- | Materialization type class.
class Mat a b | a -> b where
  mat :: Shared -> a -> Materializer b

runMaterializer
  :: SMat.StaticProperties
  -> Materializer a
  -> IO (Env, a)
runMaterializer staticProps m = do
  sharedProps <- liftIO $ newTVarIO Map.empty
  let env = Env staticProps sharedProps
  res <- runReaderT m env
  pure (env, res)

mat' :: Mat a b => SMat.StaticProperties -> a -> IO b
mat' staticProps statModel = do
  (_, a) <- runMaterializer staticProps (mat False statModel)
  pure a
