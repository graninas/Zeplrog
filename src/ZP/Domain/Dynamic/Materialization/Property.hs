{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Materialization.Property where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import ZP.Domain.Static.Materialization ()
import ZP.Domain.Static.Materialization.Materializer
import qualified ZP.Domain.Static.Query as SQuery
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Materialization.Materializer
import ZP.Domain.Dynamic.Materialization.Common
import ZP.Domain.Dynamic.Materialization.Script

import Data.Proxy
import qualified Data.Map.Strict as Map


-- | Verifies if the shared property already exists.
-- Call spawnProperty to finalize creation of the prop.
withShared
  :: Bool
  -> SMod.PropertyGroupVL
  -> DMaterializer Property
  -> DMaterializer (Essence, Property)
withShared shared group matProp = do
  sharedVar   <- asks deSharedPropertiesVar
  sharedProps <- readTVarIO sharedVar
  (ess, sId) <- dMat False () group
  case (shared, Map.lookup sId sharedProps) of
    (True, Nothing) -> do
      prop <- matProp
      atomically $ writeTVar sharedVar
                 $ Map.insert sId (ess, prop) sharedProps
      pure (ess, prop)
    (True,  Just (_, prop)) -> pure (ess, prop)
    (False, Just (pId, _)) -> error
      $ "Not shared dynamic property found in shared props: "
        <> show ess <> ", pId: " <> show pId
    (False, Nothing) -> do
      prop <- matProp
      pure (ess, prop)

---------- Materialization --------------

-- Materialize property

instance
  DMat () SMod.PropertyVL Property where
  dMat shared () (SMod.PropDict group propKVs) =
    spawnProperty $ withShared shared group $ do
      let (_, sId) = SQuery.getComboPropertyId group
      propId      <- getNextPropertyId
      props       <- mapM (dMat False ()) propKVs
      scriptVar   <- newTVarIO Nothing
      propBagsVar <- newTVarIO $ Map.fromList props
      pure $ Property
        propId
        (error "owner not implemented")         -- TODO: owner
        sId
        scriptVar
        propBagsVar

  dMat shared () (SMod.PropVal group valDef) =
    spawnProperty $ withShared shared group $ do
      let (_, sId) = SQuery.getComboPropertyId group
      propId <- getNextPropertyId
      val    <- dMat False () valDef
      valVar <- newTVarIO val
      pure $ ValueProperty propId sId valVar

  dMat _ () (SMod.StaticProp group) = do
    let (_, sId) = SQuery.getComboPropertyId group
    spawnProperty $ withShared True group $ do
      propId <- getNextPropertyId
      pure $ RefProperty propId $ StaticPropertyRef sId

  dMat _ () (SMod.StaticPropRef prop) = do
    let group    = SQuery.getGroup prop
    let (_, sId) = SQuery.getComboPropertyId group
    spawnProperty $ withShared True group $ do
      propId <- getNextPropertyId
      pure $ RefProperty propId $ StaticPropertyRef sId

  dMat _ () (SMod.PropScript group script) =
    spawnProperty $ withShared True group $ do
      let (_, sId) = SQuery.getComboPropertyId group
      propBagsVar <- newTVarIO Map.empty
      scriptVar   <- newTVarIO $ Just $ Script script
      propId      <- getNextPropertyId
      pure $ Property
        propId
        (error "owner not implemented")
        sId
        scriptVar
        propBagsVar

instance
  DMat () SMod.PropertyOwningVL (Category, PropertyOwning) where
  dMat _ () (SMod.OwnProp statProp) = do
    prop     <- dMat False () statProp
    category <- getPropertyEssence $ pPropertyId prop
    pure (category, OwnProperty prop)
  dMat _ () (SMod.SharedProp statProp) = do
    prop <- dMat True () statProp
    category <- getPropertyEssence $ pPropertyId prop
    pure (category
         , SharedProperty $ DynamicPropertyRef $ pPropertyId prop)

instance
  DMat () SMod.PropertyKeyValueVL (Category, PropertyBag) where
  dMat _ () (SMod.PropKeyBag statEss statOwnings) = do
    parentCategory <- dMat False () statEss
    ownings        <- mapM (dMat False ()) statOwnings
    owningsVar     <- newTVarIO $ Map.fromList ownings
    pure (parentCategory, PropertyDict owningsVar)
  dMat _ () (SMod.PropKeyVal statEss statOwning) = do
    parentCategory <- dMat False () statEss
    (_, owning)    <- dMat False () statOwning
    pure (parentCategory, SingleProperty owning)

--------- Special instantiation ------------------------

instance
  DMat () (Instantiate, SMod.PropertyVL) Property where
  dMat _ () (InstantiateValue path valDef, statProp) = do
    val  <- dMat False () valDef
    -- Assuming that statProp contains a `derive` value.
    let statProp' = instantiateDerivedValue path statProp

    dMat False () statProp'




