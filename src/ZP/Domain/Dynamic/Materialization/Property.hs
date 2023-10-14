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
  -> SMod.PropertyRootVL
  -> DMaterializer Property
  -> DMaterializer (Essence, Property)
withShared shared root matProp = do
  esssVar <- asks deEssencesVar
  esss    <- readTVarIO esssVar
  ess     <- dMat False () root
  case (shared, Map.lookup ess esss) of
    (True, Nothing) -> do
      prop <- matProp
      pure (ess, prop)
    (True,  Just (_, found)) -> pure (ess, found)
    (False, Just (pId, found)) -> error
      $ "Not shared dynamic property already exists: "
        <> show ess <> ", pId: " <> show pId
    (False, Nothing) -> do
      prop <- matProp
      pure (ess, prop)

---------- Materialization --------------

-- Materialize property

instance
  DMat () SMod.PropertyVL Property where
  dMat shared () (SMod.PropDict root propKVs) =
    spawnProperty $ withShared shared root $ do
      (statPropId, _) <- withSMaterializer $ getStaticPropertyByRoot root
      propId          <- getNextPropertyId
      props           <- mapM (dMat False ()) propKVs
      scriptVar       <- newTVarIO Nothing
      propBagsVar     <- newTVarIO $ Map.fromList props
      pure $ Property
        propId
        (error "owner not implemented")         -- TODO: owner
        statPropId
        scriptVar
        propBagsVar

  dMat shared () (SMod.PropVal root valDef) =
    spawnProperty $ withShared shared root $ do
      (statPropId, _) <- withSMaterializer $ getStaticPropertyByRoot root
      propId          <- getNextPropertyId
      val             <- dMat False () valDef
      valVar          <- newTVarIO val
      pure $ ValueProperty propId statPropId valVar

  dMat _ () (SMod.StaticProp root) = do
    error "stat prop not implemented"

  dMat shared () (SMod.StaticPropRef prop) = do
    let root = SQuery.getRoot prop
    spawnProperty $ withShared shared root $ do

      (statPropId, _) <- withSMaterializer $ getStaticPropertyByRoot root
      propId          <- getNextPropertyId

      pure $ RefProperty propId $ StaticPropertyRef statPropId
  dMat _ () (SMod.PropScript root script) =
    spawnProperty $ withShared True root $ do
      (statPropId, _) <- withSMaterializer $ getStaticPropertyByRoot root
      propBagsVar <- newTVarIO Map.empty
      scriptVar   <- newTVarIO $ Just $ Script script
      propId      <- getNextPropertyId
      pure $ Property
        propId
        (error "owner not implemented")
        statPropId
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




