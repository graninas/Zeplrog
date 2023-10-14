{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Materializer where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Static.Model
import ZP.Domain.Static.Query

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


---------- Interface ------------------

-- TODO: make Materializer thread-safe.
--  Currently, TVars do not do anything useful.

type StaticProperties = Map.Map StaticPropertyId (EssenceVL, PropertyVL)
type StaticEssences   = Map.Map EssenceVL (StaticPropertyId, PropertyVL)

data SEnv = SEnv
  { seDebugMode           :: DebugMode
  , seStaticPropertyIdVar :: TVar StaticPropertyId
  , seStaticPropertiesVar :: TVar StaticProperties
  , seStaticEssencesVar   :: TVar StaticEssences
  }

type SMaterializer a = ReaderT SEnv IO a

-- | Materialization type class.
class SMat payload a b | a -> b where
  sMat :: payload -> Proxy a -> SMaterializer b

runSMaterializer :: SEnv -> SMaterializer a -> IO a
runSMaterializer sEnv m = do
  when (seDebugMode sEnv == DebugEnabled) $ trace "\n" $ pure ()
  runReaderT m sEnv

sMat' :: SMat payload a b => SEnv -> payload -> Proxy a -> IO b
sMat' sEnv p proxy = runSMaterializer sEnv $ sMat p proxy

makeSEnv :: DebugMode -> IO SEnv
makeSEnv dbg = SEnv
  <$> pure dbg
  <*> newTVarIO (StaticPropertyId 0)
  <*> newTVarIO Map.empty
  <*> newTVarIO Map.empty

----- Utils ---------------

getStaticProperty
  :: StaticPropertyId
  -> SMaterializer (EssenceVL, PropertyVL)
getStaticProperty statPropId = do
  statPropsVar <- asks seStaticPropertiesVar
  statProps    <- readTVarIO statPropsVar
  case Map.lookup statPropId statProps of
    Nothing -> error
      $ "Static property " <> show statPropId <> " not found."
    Just prop -> pure prop

getStaticPropertyByRoot
  :: PropertyRootVL
  -> SMaterializer (StaticPropertyId, PropertyVL)
getStaticPropertyByRoot root = do
  let ess = getEssence root
  statEssVar <- asks seStaticEssencesVar
  statEsss   <- readTVarIO statEssVar
  case Map.lookup ess statEsss of
    Nothing   -> error
      $ "Static property " <> show ess <> " not found."
    Just prop -> pure prop

getNextStaticPropertyId'
  :: TVar StaticPropertyId
  -> SMaterializer StaticPropertyId
getNextStaticPropertyId' statPropIdVar = atomically $ do
  StaticPropertyId pId <- readTVar statPropIdVar
  writeTVar statPropIdVar $ StaticPropertyId $ pId + 1
  pure $ StaticPropertyId pId

getNextStaticPropertyId :: SMaterializer StaticPropertyId
getNextStaticPropertyId = do
  statPropIdVar <- asks seStaticPropertyIdVar
  getNextStaticPropertyId' statPropIdVar


addStaticProperty
  :: (StaticPropertyId, EssenceVL, PropertyVL)
  -> SMaterializer ()
addStaticProperty (statPropId, ess, prop) = do
  statPropsVar    <- asks seStaticPropertiesVar
  statEssencesVar <- asks seStaticEssencesVar
  atomically $ do
    props <- readTVar statPropsVar
    esss  <- readTVar statEssencesVar
    writeTVar statPropsVar    $ Map.insert statPropId (ess, prop) props
    writeTVar statEssencesVar $ Map.insert ess (statPropId, prop) esss


sTraceDebug :: String -> SMaterializer ()
sTraceDebug msg = do
  dbg <- asks seDebugMode
  when (dbg == DebugEnabled) $ trace msg $ pure ()


