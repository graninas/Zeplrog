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

type DynamicProperties = Map.Map PropertyId Property

data DEnv = DEnv
  SEnv
  (TVar PropertyId)  -- ^ PropId counter
  (TVar ObjectId)    -- ^ ObjectId counter
  (TVar DynamicProperties)  -- ^ List of all dynamic props

type DMaterializer a = ReaderT DEnv IO a

type Shared = Bool

-- | Materialization type class.
class DMat payload a b | a -> b where
  dMat :: Shared -> payload -> a -> DMaterializer b

runDMaterializer :: DEnv -> DMaterializer a -> IO a
runDMaterializer dEnv m = runReaderT m dEnv

dMat' :: DMat payload a b => DEnv -> payload -> a -> IO b
dMat' dEnv p itVL = runDMaterializer dEnv $ dMat False p itVL

fullMat
  :: SMat payload itTL itVL
  => DMat payload itVL res
  => DEnv
  -> payload
  -> Proxy itTL
  -> IO res
fullMat dEnv@(DEnv sEnv _ _ _) p proxy = do
  itVL <- sMat' sEnv p proxy
  dMat' dEnv p itVL

makeDEnv :: SEnv -> IO DEnv
makeDEnv sEnv = DEnv
  <$> pure sEnv
  <*> newTVarIO (PropertyId 0)
  <*> newTVarIO 0
  <*> newTVarIO Map.empty

makeEnvs :: DebugMode -> IO (SEnv, DEnv)
makeEnvs dbg = do
  sEnv <- makeSEnv dbg
  dEnv <- makeDEnv sEnv
  pure (sEnv, dEnv)
