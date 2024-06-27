{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module ZP.Domain.Dynamic.Instantiation.Instantiator where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Static.Materialization.Materializer
import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Query as SQuery
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Dynamic.Model

import qualified Data.Map.Strict as Map


---------- Dynamic instantiation interface ------------------

type DynamicProperties = Map.Map PropertyId (Essence, Property)
type SharedProperties  = Map.Map SMod.StaticPropertyId (Essence, Property)
type DynamicEssences   = Map.Map Essence [(PropertyId, Property)]

data DEnv = DEnv
  { deSEnv                :: SMat.SEnv
    -- ^ Static environment
  , dePropertyIdRef       :: IORef PropertyId
    -- ^ PropId counter
  , dePropertiesRef       :: IORef DynamicProperties
    -- ^ List of all dynamic props
  , deSharedPropertiesRef :: IORef SharedProperties
    -- ^ List of shared props
  , deEssencesRef         :: IORef DynamicEssences
    -- ^ List of all dynamic props
  , deObjectIdRef         :: IORef ObjectId
  }

type DInstantiator a = ReaderT DEnv IO a

type Shared = Bool

-- | Instantiation type class.
class DInst payload a b | payload a -> b where
  dInst :: Shared -> payload -> a -> DInstantiator b

runDInstantiator :: DEnv -> DInstantiator a -> IO a
runDInstantiator dEnv m = runReaderT m dEnv

dInst'
  :: DInst payload a b
  => DEnv
  -> payload
  -> a
  -> IO b
dInst' dEnv p itVL = runDInstantiator dEnv $ dInst False p itVL

dInstParent
  :: DInst (Maybe PropertyId) SMod.PropertyVL (Essence, Property)
  => DEnv
  -> Maybe PropertyId
  -> SMod.PropertyVL
  -> IO Property
dInstParent dEnv mbParentId pVL = do
  (_ :: Essence, prop) <- runDInstantiator dEnv $ dInst False mbParentId pVL
  pure prop

fullInst
  :: SMat.SMat payload itTL itVL
  => DInst payload itVL res
  => DEnv
  -> payload
  -> Proxy itTL
  -> IO res
fullInst dEnv p proxy = do
  itVL <- SMat.sMat' (deSEnv dEnv) p proxy
  dInst' dEnv p itVL

makeDEnv :: SMat.SEnv -> IO DEnv
makeDEnv sEnv = DEnv
  <$> pure sEnv
  <*> newIORef (PropertyId 0)
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef (ObjectId 0)

makeEnvs :: DebugMode -> IO (SMat.SEnv, DEnv)
makeEnvs dbg = do
  sEnv <- SMat.makeSEnv dbg
  dEnv <- makeDEnv sEnv
  pure (sEnv, dEnv)

---------- Utils -----------------

getNextPropertyId'
  :: IORef PropertyId
  -> DInstantiator PropertyId
getNextPropertyId' propIdRef = do
  PropertyId pId <- readIORef propIdRef
  writeIORef propIdRef $ PropertyId $ pId + 1
  pure $ PropertyId pId

getNextPropertyId :: DInstantiator PropertyId
getNextPropertyId = do
  propIdRef <- asks dePropertyIdRef
  getNextPropertyId' propIdRef

getPropertyEssence :: PropertyId -> DInstantiator Essence
getPropertyEssence propId = do
  propsRef <- asks dePropertiesRef
  props    <- readIORef propsRef
  case Map.lookup propId props of
    Nothing -> error
      $ "Property essence not found for pId: " <> show propId
    Just (ess, _) -> pure ess

dTraceDebug :: DInstantiator String -> DInstantiator ()
dTraceDebug mMsg = do
  dbg <- asks $ SMat.seDebugMode . deSEnv
  when (dbg == DebugEnabled) $ do
    msg <- mMsg
    trace msg $ pure ()

withSMaterializer
  :: SMat.SMaterializer a
  -> DInstantiator a
withSMaterializer sMaterializer = do
  sEnv <- asks deSEnv
  liftIO $ SMat.runSMaterializer sEnv sMaterializer

-- | Finalizes creation of property.
-- Registers the property in maps.
spawnProperty
  :: DInstantiator (Essence, Property)
  -> DInstantiator (Essence, Property)
spawnProperty propMat = do
  (ess, prop) <- propMat
  let propId = pPropertyId prop

  propsRef  <- asks dePropertiesRef
  esssRef   <- asks deEssencesRef

  pId <- do
    props  <- readIORef propsRef
    esss   <- readIORef esssRef

    let props'  = Map.insert propId (ess, prop) props
    writeIORef propsRef props'

    case Map.lookup ess esss of
      Nothing -> writeIORef esssRef
        $ Map.insert ess [(propId, prop)] esss
      Just ps -> writeIORef esssRef
        $ Map.insert ess ((propId, prop) : ps) esss


  dTraceDebug $ do
    pure $ "Dyn property created: "
      <> " "
      <> show ess
      <> ", pId: "
      <> show pId

  pure (ess, prop)

spawnObject :: Property -> DInstantiator Object
spawnObject prop = do
  objIdRef <- asks deObjectIdRef
  ObjectId objId <- readIORef objIdRef
  writeIORef objIdRef $ ObjectId $ objId + 1
  pure $ Object (ObjectId objId) prop

fullMat
  :: SMat payload itTL itVL
  => DInst payload itVL res
  => DEnv
  -> payload
  -> Proxy itTL
  -> IO res
fullMat dEnv p proxy = do
  itVL <- sMat' (deSEnv dEnv) p proxy
  dInst' dEnv p itVL
