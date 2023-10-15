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
type StaticEssences   = Map.Map EssenceVL [(StaticPropertyId, PropertyVL)]

data SEnv = SEnv
  { seDebugMode           :: DebugMode
  , seStaticPropertyIdVar :: TVar StaticPropertyId
  , seStaticPropertiesVar :: TVar StaticProperties
  , seStaticEssencesVar   :: TVar StaticEssences
  }

type SMaterializer a = ReaderT SEnv IO a

-- | Materialization type class.
class SMat payload a b | payload a -> b where
  sMat :: payload -> Proxy a -> SMaterializer b

-- | Special payload to make specific instances when materializing props.
data Instantiate
  = InstantiateValue
      [EssenceVL]        -- ^ Possible path to this value
      ValDefVL           -- ^ Value to instantiate

-- | Run materializer with an environment.
runSMaterializer :: SEnv -> SMaterializer a -> IO a
runSMaterializer sEnv m = do
  when (seDebugMode sEnv == DebugEnabled) $ trace "\n" $ pure ()
  runReaderT m sEnv

-- | Materialize a type.
sMat' :: SMat payload a b => SEnv -> payload -> Proxy a -> IO b
sMat' sEnv p proxy = runSMaterializer sEnv $ sMat p proxy

-- | Create the environment.
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


lookupSingletonProperty
  :: EssenceVL
  -> SMaterializer (Maybe PropertyVL)
lookupSingletonProperty ess = do
  staticEssencesVar <- asks seStaticEssencesVar
  esss <- readTVarIO staticEssencesVar
  case Map.lookup ess esss of
    Just [(sId, prop)] -> pure $ Just prop
    Just (_:_:_)       -> error $ "Multiple properties for singleton prop found, ess: " <> show ess
    Nothing            -> pure Nothing

addStaticProperty
  :: (StaticPropertyId, EssenceVL, PropertyVL)
  -> SMaterializer ()
addStaticProperty (statPropId, ess, prop) = do
  statPropsVar    <- asks seStaticPropertiesVar
  statEssencesVar <- asks seStaticEssencesVar
  atomically $ do
    props <- readTVar statPropsVar
    esss  <- readTVar statEssencesVar

    writeTVar statPropsVar
      $ Map.insert statPropId (ess, prop) props

    case Map.lookup ess esss of
      Nothing -> writeTVar statEssencesVar
        $ Map.insert ess [(statPropId, prop)] esss
      Just ps -> writeTVar statEssencesVar
        $ Map.insert ess ((statPropId, prop) : ps) esss


instantiateDerivedValue
  :: [EssenceVL]
  -> PropertyVL
  -> PropertyVL
instantiateDerivedValue path statProp = error "not implemented"



sTraceDebug :: String -> SMaterializer ()
sTraceDebug msg = do
  dbg <- asks seDebugMode
  when (dbg == DebugEnabled) $ trace msg $ pure ()


