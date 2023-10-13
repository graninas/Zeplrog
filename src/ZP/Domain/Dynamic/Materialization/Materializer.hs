{-# LANGUAGE DataKinds #-}
module ZP.Domain.Dynamic.Materialization.Materializer where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Static.Materialization.Materializer
import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Dynamic.Model

import qualified Data.Map.Strict as Map


---------- Dynamic materializer interface ------------------

-- TODO: make Materializer thread-safe.
--  Currently, TVars do not do anything useful.

type DynamicProperties = Map.Map PropertyId Property

data DEnv = DEnv
  SEnv
  (TVar PropertyId)         -- ^ PropId counter
  (TVar ObjectId)           -- ^ ObjectId counter
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
  <*> newTVarIO (ObjectId 0)
  <*> newTVarIO Map.empty

makeEnvs :: DebugMode -> IO (SEnv, DEnv)
makeEnvs dbg = do
  sEnv <- makeSEnv dbg
  dEnv <- makeDEnv sEnv
  pure (sEnv, dEnv)

---------- Utils -----------------

getNextPropertyId'
  :: TVar PropertyId
  -> DMaterializer PropertyId
getNextPropertyId' propIdVar = atomically $ do
  PropertyId pId <- readTVar propIdVar
  writeTVar propIdVar $ PropertyId $ pId + 1
  pure $ PropertyId pId

getNextPropertyId :: DMaterializer PropertyId
getNextPropertyId = do
  DEnv _ propIdVar _ _ <- ask
  getNextPropertyId' propIdVar


getStaticPropertyId
  :: SMod.PropertyRootVL
  -> SMod.StaticPropertyId
getStaticPropertyId root = do
  let ess = SMat.getEssence root
  DEnv (SMat.SEnv _ _ _ statEssVar) _ _ _ <- ask

  statEsss <- readTVarIO statEssVar
  case Map.lookup ess statEsss of
    Nothing -> error $ "Static property " <> show ess <> " not found."
    Just (statPropId, _) -> pure statPropId

getStaticProperty
  :: SMod.StaticPropertyId
  -> (SMod.EssenceVL, SMod.PropertyVL)
getStaticProperty statPropId = do
  DEnv (SMat.SEnv _ _ statPropsVar _) _ _ _ <- ask

  statProps <- readTVarIO statPropsVar
  case Map.lookup statPropId statProps of
    Nothing -> error $ "Static property " <> show statPropId <> " not found."
    Just res -> pure res

traceDebug :: DMaterializer String -> DMaterializer ()
traceDebug mMsg = do
  DEnv (SMat.SEnv dbg _ _ _) _ _ _ <- ask
  when (dbg == DebugEnabled) $ do
    msg <- mMsg
    trace msg $ pure ()

spawnProperty :: DMaterializer Property -> DMaterializer Property
spawnProperty propMat = do
  prop <- propMat

  DEnv _ _ _ propsVar <- ask

  pId <- atomically $ do
    let pId = pPropertyId prop
    props <- readTVar propsVar
    writeTVar $ Map.insert pId prop
    pure pId

  traceDebug $ do
    (statEss, _) <- getStaticProperty $ pStaticPropertyId prop
    pure $ "Dyn property created: "
      <> show pId
      <> " for static property: "
      <> show statEss

  pure prop
