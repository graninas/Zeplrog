{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materializer where

import ZP.Prelude

import ZP.Domain.Static.Model

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


---------- Interface ------------------

type StaticProperties =
  Map.Map (Essence 'ValueLevel) (Property 'ValueLevel)

newtype Env = Env (IORef StaticProperties)

type Materializer a = ReaderT Env IO a

-- | Materialization type class.
class Mat a b | a -> b where
  mat :: Proxy a -> Materializer b

runMaterializer :: Materializer a -> IO (Env, a)
runMaterializer m = do
  staticProps <- liftIO $ newIORef Map.empty
  let env = Env staticProps
  res <- runReaderT m env
  pure (env, res)

mat' :: Mat a b => Proxy a -> IO b
mat' proxy = do
  (_, a) <- runMaterializer (mat proxy)
  pure a
