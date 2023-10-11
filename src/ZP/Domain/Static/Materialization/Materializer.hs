{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Materializer where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Static.Model

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


---------- Interface ------------------

-- TODO: make Materializer thread-safe (STM instead of IO)

type StaticProperties =
  Map.Map (Essence 'ValueLevel) (Property 'ValueLevel)

data SEnv = SEnv DebugMode (TVar StaticProperties)

type SMaterializer a = ReaderT SEnv IO a

-- | Materialization type class.
class SMat a b | a -> b where
  sMat :: Proxy a -> SMaterializer b

runSMaterializer :: SEnv -> SMaterializer a -> IO a
runSMaterializer sEnv m = runReaderT m sEnv

sMat' :: SMat a b => SEnv -> Proxy a -> IO b
sMat' sEnv proxy = runSMaterializer sEnv $ sMat proxy

makeSEnv :: DebugMode -> IO SEnv
makeSEnv dbg = SEnv
  <$> pure dbg
  <*> newTVarIO Map.empty

