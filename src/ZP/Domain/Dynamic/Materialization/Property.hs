{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Materialization.Property where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import ZP.Domain.Static.Materialization ()
import ZP.Domain.Static.Materialization.Materializer
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
  -> payload
  -> SMod.PropertyRootVL
  -> DMaterializer Property
  -> DMaterializer (Essence, Property)
withShared shared p root matProp = do
  esssVar <- asks deEssencesVar
  esss    <- readTVarIO esssVar
  ess     <- dMat False p root
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
  DMat p SMod.PropertyVL Property where
  dMat shared p (SMod.PropDict root propKVs) =
    spawnProperty $ withShared shared p root $ do
      (statPropId, _) <- withSMaterializer $ getStaticPropertyByRoot root
      propId          <- getNextPropertyId
      props           <- mapM (dMat False p) propKVs
      scriptVar       <- newTVarIO Nothing
      propBagsVar     <- newTVarIO $ Map.fromList props
      pure $ Property
        propId
        (error "owner not implemented")         -- TODO: owner
        statPropId
        scriptVar
        propBagsVar

  dMat shared p (SMod.PropVal root valDef) =
    spawnProperty $ withShared shared p root $ do
      (statPropId, _) <- withSMaterializer $ getStaticPropertyByRoot root
      propId          <- getNextPropertyId
      val             <- dMat False p valDef
      valVar          <- newTVarIO val
      pure $ ValueProperty propId statPropId valVar

  dMat _ p (SMod.StaticProp root) = do
    error "stat prop not implemented"

  dMat _ p (SMod.StaticPropRef prop) = do
    error "static prop ref not implemented"
    -- let SMod.StaticProp root = prop
    -- let statEss = getEssence root

    -- DEnv (SEnv _ _ statPropsVar) propIdVar objIdVar _ <- ask
    -- unless (Map.member statEss statProps)
    --   $ error $ "Static property not found: " <> show statEss

    -- ess <- dMat False p statEss
    -- let staticProp = StaticPropertyRef root
    -- valVar <- newTVarIO $ StaticPropertyRefValue root
    -- pure (ess, ValueProperty ess staticProp valVar)

  dMat _ p (SMod.PropScript root script) =
    spawnProperty $ withShared True p root $ do
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
  DMat p SMod.PropertyOwningVL (Category, PropertyOwning) where
  dMat _ p (SMod.OwnProp statProp) = do
    prop     <- dMat False p statProp
    category <- getPropertyEssence $ pPropertyId prop
    pure (category, OwnProperty prop)
  dMat _ p (SMod.SharedProp statProp) = do
    prop <- dMat True p statProp
    category <- getPropertyEssence $ pPropertyId prop
    pure (category
         , SharedProperty $ DynamicPropertyRef $ pPropertyId prop)

instance
  DMat p SMod.PropertyKeyValueVL (Category, PropertyBag) where
  dMat _ p (SMod.PropKeyBag statEss statOwnings) = do
    parentCategory <- dMat False p statEss
    ownings        <- mapM (dMat False p) statOwnings
    owningsVar     <- newTVarIO $ Map.fromList ownings
    pure (parentCategory, PropertyDict owningsVar)
  dMat _ p (SMod.PropKeyVal statEss statOwning) = do
    parentCategory <- dMat False p statEss
    (_, owning)    <- dMat False p statOwning
    pure (parentCategory, SingleProperty owning)

