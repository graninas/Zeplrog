{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module ZP.Domain.Dynamic.Instantiation.Property where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import ZP.Domain.Static.Materialization ()
import ZP.Domain.Static.Materialization.Materializer
import qualified ZP.Domain.Static.Query as SQ
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Instantiation.Instantiator
import ZP.Domain.Dynamic.Instantiation.Common
import qualified ZP.Domain.Dynamic.Instantiation.Script as Script

import Data.Proxy
import qualified Data.Map.Strict as Map



-- | Verifies if the shared property already exists.
-- Call spawnProperty to finalize creation of the prop.
withShared
  :: Bool
  -> SMod.PropertyGroupVL
  -> DInstantiator (DEssence, Property)
  -> DInstantiator (DEssence, Property)
withShared shared group matProp = do
  sharedVar   <- asks deSharedPropertiesRef
  sharedProps <- readIORef sharedVar
  (ess, sId) <- dInst False () group
  case (shared, Map.lookup sId sharedProps) of
    (True, Nothing) -> do
      (_, prop) <- matProp
      writeIORef sharedVar
        $ Map.insert sId (ess, prop) sharedProps
      pure (ess, prop)
    (True,  Just (_, prop)) -> pure (ess, prop)
    (False, Just (pId, _)) -> error
      $ "Not shared dynamic property found in shared props: "
        <> show ess <> ", pId: " <> show pId
    (False, Nothing) -> do
      (_, prop) <- matProp
      pure (ess, prop)

---------- Instantiation --------------

-- Instantiate property

type MbParentId = Maybe PropertyId

instance
  DInst MbParentId SMod.PropertyVL (DEssence, Property) where
  dInst shared _ (SMod.TagPropRef tagProp) = do
    ess <- dInst False () $ SQ.getEssence tagProp
    pure $ (ess, TagPropRef ess tagProp)

  dInst shared mbParentId (SMod.PropDict group propKVs scripts) =
    spawnProperty $ withShared shared group $ do
      let (sEss, sId) = SQ.getComboId group

      ess         <- dInst False () sEss
      propId      <- getNextPropertyId
      props       <- mapM (dInst False (Just propId)) propKVs
      propBagsVar <- newIORef $ Map.fromList props

      -- Prop without scripts
      let tmpProp = Prop propId ess mbParentId sId propBagsVar Map.empty

      scripts' <- liftIO $ mapM (Script.makeScript tmpProp) scripts
      pure (ess, tmpProp {pScripts = Map.fromList scripts'})


instance
  DInst MbParentId SMod.PropertyOwningVL PropertyOwning where

  dInst _ _ (SMod.OwnVal valDef) = do
    let val = fromJust $ SQ.queryValue (SMod.RelPath []) valDef
    valRef <- newIORef val
    pure (OwnVal valRef)

  dInst _ mbParentId (SMod.OwnProp statProp) = do
    (_, prop) <- dInst False mbParentId statProp
    pure (OwnProp prop)

  dInst _ mbParentId (SMod.SharedProp statProp) = do
    (_, prop) <- dInst True mbParentId statProp
    -- FIXME: there is a bug here. `pPropertyId` is not working
    --  for all cases
    pure (SharedProp $ DynamicPropRef $ pPropertyId prop)

instance
  DInst MbParentId SMod.PropertyKeyValueVL (DEssence, PropertyOwning) where

  dInst _ mbParentId (SMod.PropKeyVal keyEss statOwning) = do
    keyEss <- dInst False () keyEss
    owning <- dInst False mbParentId statOwning
    pure (keyEss, owning)

  dInst _ mbParentId (SMod.PropKeyBag keyEss props) = do
    keyEss   <- dInst False () keyEss
    props    <- mapM (dInst False mbParentId) props
    propsVar <- newIORef $ Map.fromList props
    pure (keyEss, OwnDict propsVar)


instance
  DInst () SMod.PropertyVL Property where
  dInst _ _ sProp = instProperty sProp

instProperty
  :: SMod.PropertyVL
  -> DInstantiator Property
instProperty sProp = do
  (_ :: DEssence, prop)
      <- dInst False (Nothing :: MbParentId) sProp
  pure prop
