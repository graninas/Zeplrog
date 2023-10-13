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

type StaticProperties = Map.Map EssenceVL PropertyVL

data SEnv = SEnv DebugMode (TVar StaticProperties)

type SMaterializer a = ReaderT SEnv IO a

-- | Materialization type class.
class SMat payload a b | a -> b where
  sMat :: payload -> Proxy a -> SMaterializer b

runSMaterializer :: SEnv -> SMaterializer a -> IO a
runSMaterializer sEnv@(SEnv dbg _) m = do
  when (dbg == DebugEnabled) $ trace "\n" $ pure ()
  runReaderT m sEnv

sMat' :: SMat payload a b => SEnv -> payload -> Proxy a -> IO b
sMat' sEnv p proxy = runSMaterializer sEnv $ sMat p proxy

makeSEnv :: DebugMode -> IO SEnv
makeSEnv dbg = SEnv
  <$> pure dbg
  <*> newTVarIO Map.empty

