{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Instantiation.PropertyOld where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import ZP.Domain.Static.Materialization ()
import ZP.Domain.Static.Materialization.Materializer
import qualified ZP.Domain.Static.Query as SQuery
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Instantiation.Instantiator
import ZP.Domain.Dynamic.Instantiation.Common
import ZP.Domain.Dynamic.Instantiation.Script

import Data.Proxy
import qualified Data.Map.Strict as Map


-- -- | Verifies if the shared property already exists.
-- -- Call spawnProperty to finalize creation of the prop.
-- withShared
--   :: Bool
--   -> SMod.PropertyGroupVL
--   -> DInstantiator Property
--   -> DInstantiator (Essence, Property)
-- withShared shared group matProp = do
--   sharedVar   <- asks deSharedPropertiesVar
--   sharedProps <- readTVarIO sharedVar
--   (ess, sId) <- dInst False () group
--   case (shared, Map.lookup sId sharedProps) of
--     (True, Nothing) -> do
--       prop <- matProp
--       atomically $ writeTVar sharedVar
--                  $ Map.insert sId (ess, prop) sharedProps
--       pure (ess, prop)
--     (True,  Just (_, prop)) -> pure (ess, prop)
--     (False, Just (pId, _)) -> error
--       $ "Not shared dynamic property found in shared props: "
--         <> show ess <> ", pId: " <> show pId
--     (False, Nothing) -> do
--       prop <- matProp
--       pure (ess, prop)

-- ---------- Materialization --------------

-- -- Materialize property

-- instance
--   DInst () SMod.PropertyVL Property where
--   dInst shared () (SMod.PropDict group propKVs) =
--     spawnProperty $ withShared shared group $ do
--       let (_, sId) = SQuery.getComboPropertyId group
--       propId      <- getNextPropertyId
--       props       <- mapM (dInst False ()) propKVs
--       scriptVar   <- newTVarIO Nothing
--       propBagsVar <- newTVarIO $ Map.fromList props
--       pure $ Property
--         propId
--         (error "owner not implemented")         -- TODO: owner
--         sId
--         scriptVar
--         propBagsVar

--   dInst shared () (SMod.PropVal group valDef) =
--     spawnProperty $ withShared shared group $ do
--       let (_, sId) = SQuery.getComboPropertyId group
--       propId <- getNextPropertyId
--       val    <- dInst False () valDef
--       valVar <- newTVarIO val
--       pure $ ValueProperty propId sId valVar

--   dInst _ () (SMod.StaticProp group) = do
--     let (_, sId) = SQuery.getComboPropertyId group
--     spawnProperty $ withShared True group $ do
--       propId <- getNextPropertyId
--       pure $ RefProperty propId $ StaticPropertyRef sId

--   dInst _ () (SMod.StaticPropRef prop) = do
--     let group    = SQuery.getGroup prop
--     let (_, sId) = SQuery.getComboPropertyId group
--     spawnProperty $ withShared True group $ do
--       propId <- getNextPropertyId
--       pure $ RefProperty propId $ StaticPropertyRef sId

--   dInst _ () (SMod.PropScript group script) =
--     spawnProperty $ withShared True group $ do
--       let (_, sId) = SQuery.getComboPropertyId group
--       propBagsVar <- newTVarIO Map.empty
--       scriptVar   <- newTVarIO $ Just $ Script script
--       propId      <- getNextPropertyId
--       pure $ Property
--         propId
--         (error "owner not implemented")
--         sId
--         scriptVar
--         propBagsVar

-- instance
--   DInst () SMod.PropertyOwningVL (Category, PropertyOwning) where
--   dInst _ () (SMod.OwnProp statProp) = do
--     prop     <- dInst False () statProp
--     category <- getPropertyEssence $ pPropertyId prop
--     pure (category, OwnProperty prop)
--   dInst _ () (SMod.SharedProp statProp) = do
--     prop <- dInst True () statProp
--     category <- getPropertyEssence $ pPropertyId prop
--     pure (category
--          , SharedProperty $ DynamicPropertyRef $ pPropertyId prop)

-- instance
--   DInst () SMod.PropertyKeyValueVL (Category, PropertyBag) where
--   dInst _ () (SMod.PropKeyBag statEss statOwnings) = do
--     parentCategory <- dInst False () statEss
--     ownings        <- mapM (dInst False ()) statOwnings
--     owningsVar     <- newTVarIO $ Map.fromList ownings
--     pure (parentCategory, PropertyDict owningsVar)
--   dInst _ () (SMod.PropKeyVal statEss statOwning) = do
--     parentCategory <- dInst False () statEss
--     (_, owning)    <- dInst False () statOwning
--     pure (parentCategory, SingleProperty owning)

-- --------- Special instantiation ------------------------




