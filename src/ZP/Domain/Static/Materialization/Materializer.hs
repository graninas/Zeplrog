{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Materializer where

import ZP.Prelude

import ZP.Domain.Static.Model

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


---------- Interface ------------------

type StaticProperties =
  Map.Map (Essence 'ValueLevel) (Property 'ValueLevel)

data DebugMode
  = DebugEnabled
  | DebugDisabled
  deriving (Eq)

data Env = Env DebugMode (IORef StaticProperties)

type Materializer a = ReaderT Env IO a

-- | Materialization type class.
class Mat a b | a -> b where
  mat :: Proxy a -> Materializer b

runMaterializer :: DebugMode -> Materializer a -> IO (Env, a)
runMaterializer dbg m = do
  staticProps <- liftIO $ newIORef Map.empty
  let env = Env dbg staticProps
  res <- runReaderT m env
  pure (env, res)

mat' :: DebugMode -> Mat a b => Proxy a -> IO b
mat' dbg proxy = do
  (_, a) <- runMaterializer dbg (mat proxy)
  pure a
