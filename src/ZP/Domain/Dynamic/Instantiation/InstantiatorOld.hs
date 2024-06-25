{-# LANGUAGE DataKinds #-}
module ZP.Domain.Dynamic.Instantiation.InstantiatorOld where

import ZP.Prelude

-- import ZP.System.Debug
-- import ZP.Domain.Static.Materialization.Materializer
-- import qualified ZP.Domain.Static.Model as SMod
-- import qualified ZP.Domain.Static.Query as SQuery
-- import qualified ZP.Domain.Static.Materialization as SMat
-- import ZP.Domain.Dynamic.Model

-- import qualified Data.Map.Strict as Map


-- ---------- Dynamic materializer interface ------------------

-- -- TODO: make Materializer thread-safe.
-- --  Currently, TVars do not do anything useful.

-- type DynamicProperties = Map.Map PropertyId (Essence, Property)
-- type SharedProperties  = Map.Map SMod.StaticPropertyId (Essence, Property)
-- type DynamicEssences   = Map.Map Essence [(PropertyId, Property)]

-- data DEnv = DEnv
--   { deSEnv :: SEnv
--     -- ^ Static environment
--   , dePropertyIdVar       :: TVar PropertyId
--     -- ^ PropId counter
--   , deObjectIdVar         :: TVar ObjectId
--     -- ^ ObjectId counter
--   , dePropertiesVar       :: TVar DynamicProperties
--     -- ^ List of all dynamic props
--   , deSharedPropertiesVar :: TVar SharedProperties
--     -- ^ List of shared props
--   , deEssencesVar         :: TVar DynamicEssences
--     -- ^ List of all dynamic props
--   }

-- type DInstantiator a = ReaderT DEnv IO a

-- type Shared = Bool

-- -- | Materialization type class.
-- class DInst payload a b | payload a -> b where
--   dInst :: Shared -> payload -> a -> DInstantiator b

-- runDInstantiator :: DEnv -> DInstantiator a -> IO a
-- runDInstantiator dEnv m = runReaderT m dEnv

-- dInst' :: DInst payload a b => DEnv -> payload -> a -> IO b
-- dInst' dEnv p itVL = runDInstantiator dEnv $ dInst False p itVL

-- fullMat
--   :: SMat payload itTL itVL
--   => DInst payload itVL res
--   => DEnv
--   -> payload
--   -> Proxy itTL
--   -> IO res
-- fullMat dEnv p proxy = do
--   itVL <- sMat' (deSEnv dEnv) p proxy
--   dInst' dEnv p itVL

-- makeDEnv :: SEnv -> IO DEnv
-- makeDEnv sEnv = DEnv
--   <$> pure sEnv
--   <*> newTVarIO (PropertyId 0)
--   <*> newTVarIO (ObjectId 0)
--   <*> newTVarIO Map.empty
--   <*> newTVarIO Map.empty
--   <*> newTVarIO Map.empty

-- makeEnvs :: DebugMode -> IO (SEnv, DEnv)
-- makeEnvs dbg = do
--   sEnv <- makeSEnv dbg
--   dEnv <- makeDEnv sEnv
--   pure (sEnv, dEnv)

-- ---------- Utils -----------------

-- getNextPropertyId'
--   :: TVar PropertyId
--   -> DInstantiator PropertyId
-- getNextPropertyId' propIdVar = atomically $ do
--   PropertyId pId <- readTVar propIdVar
--   writeTVar propIdVar $ PropertyId $ pId + 1
--   pure $ PropertyId pId

-- getNextPropertyId :: DInstantiator PropertyId
-- getNextPropertyId = do
--   propIdVar <- asks dePropertyIdVar
--   getNextPropertyId' propIdVar

-- getPropertyEssence :: PropertyId -> DInstantiator Essence
-- getPropertyEssence propId = do
--   propsVar <- asks dePropertiesVar
--   props    <- readTVarIO propsVar
--   case Map.lookup propId props of
--     Nothing -> error
--       $ "Property essence not found for pId: " <> show propId
--     Just (ess, _) -> pure ess

-- dTraceDebug :: DInstantiator String -> DInstantiator ()
-- dTraceDebug mMsg = do
--   dbg <- asks $ seDebugMode . deSEnv
--   when (dbg == DebugEnabled) $ do
--     msg <- mMsg
--     trace msg $ pure ()


-- -- | Finalizes creation of property.
-- -- Registers the property in maps.
-- spawnProperty
--   :: DInstantiator (Essence, Property)
--   -> DInstantiator Property
-- spawnProperty propMat = do
--   (ess, prop) <- propMat
--   let propId = pPropertyId prop

--   propsVar  <- asks dePropertiesVar
--   esssVar   <- asks deEssencesVar

--   pId <- atomically $ do
--     props  <- readTVar propsVar
--     esss   <- readTVar esssVar

--     let props'  = Map.insert propId (ess, prop) props
--     writeTVar propsVar props'

--     case Map.lookup ess esss of
--       Nothing -> writeTVar esssVar
--         $ Map.insert ess [(propId, prop)] esss
--       Just ps -> writeTVar esssVar
--         $ Map.insert ess ((propId, prop) : ps) esss


--   dTraceDebug $ do
--     pure $ "Dyn property created: "
--       <> " "
--       <> show ess
--       <> ", pId: "
--       <> show pId

--   pure prop

-- spawnObject :: Property -> DInstantiator Object
-- spawnObject prop = do
--   objIdVar <- asks deObjectIdVar
--   atomically $ do
--     ObjectId objId <- readTVar objIdVar
--     writeTVar objIdVar $ ObjectId $ objId + 1
--     pure $ Object (ObjectId objId) prop

-- withSMaterializer
--   :: SMaterializer a
--   -> DInstantiator a
-- withSMaterializer sMaterializer = do
--   sEnv <- asks deSEnv
--   liftIO $ runSMaterializer sEnv sMaterializer
