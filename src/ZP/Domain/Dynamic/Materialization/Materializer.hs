{-# LANGUAGE DataKinds #-}
module ZP.Domain.Dynamic.Materialization.Materializer where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Static.Materialization.Materializer
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Dynamic.Model

import qualified Data.Map.Strict as Map


---------- Dynamic materializer interface ------------------

-- TODO: make Materializer thread-safe (STM instead of IO)

type DynamicProperties = Map.Map Essence Property

data DEnv = DEnv
  SEnv
  (TVar DynamicProperties)

type DMaterializer a = ReaderT DEnv IO a

type Shared = Bool

-- | Materialization type class.
class DMat a b | a -> b where
  dMat :: Shared -> a -> DMaterializer b

runDMaterializer :: DEnv -> DMaterializer a -> IO a
runDMaterializer dEnv m = runReaderT m dEnv

dMat' :: DMat a b => DEnv -> a -> IO b
dMat' dEnv itVL = runDMaterializer dEnv $ dMat False itVL

fullMat
  :: SMat itTL itVL
  => DMat itVL res
  => DEnv
  -> Proxy itTL
  -> IO res
fullMat dEnv@(DEnv sEnv _) proxy = do
  itVL <- sMat' sEnv proxy
  dMat' dEnv itVL

makeDEnv :: SEnv -> IO DEnv
makeDEnv sEnv = DEnv
  <$> pure sEnv
  <*> newTVarIO Map.empty

makeEnvs :: DebugMode -> IO (SEnv, DEnv)
makeEnvs dbg = do
  sEnv <- makeSEnv dbg
  dEnv <- makeDEnv sEnv
  pure (sEnv, dEnv)

