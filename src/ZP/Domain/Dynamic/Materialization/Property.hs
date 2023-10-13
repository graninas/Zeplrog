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


getNextPropertyId :: DMaterializer PropertyId
getNextPropertyId = do
  DEnv _ propIdVar _ _ <- ask
  do
      PropertyId pId <- readTVar propIdVar
      writeTVar propIdVar $ PropertyId $ pId + 1
      pure $ PropertyId pId

getEssence :: SMod.StaticPropertyRootVL -> SMod.EssenceVL
getEssence (SMod.EssStaticRoot ess) = ess
getEssence (SMod.PropStaticRoot ess _) = ess

getRoot :: SMod.PropertyVL -> SMod.StaticPropertyRootVL
getRoot (SMod.StaticProp root) = root
getRoot (SMod.StaticPropRef prop) = getRoot prop
getRoot (SMod.PropVal root _) = root
getRoot (SMod.PropDict root _) = root
getRoot (SMod.PropScript root _) = root

withShared
  :: Bool
  -> payload
  -> SMod.StaticPropertyRoot 'SMod.ValueLevel
  -> SMod.Property 'SMod.ValueLevel
  -> DMaterializer (Essence, Property)
  -> DMaterializer (Essence, Property)
withShared False p root _    matProp = matProp
withShared True  p root prop matProp = do
  ess <- dMat False p root
  DEnv _ propIdVar objIdVar propsVar <- ask
  props <- readTVarIO propsVar
  case Map.lookup ess props of
    Nothing -> do
      (_, prop) <- dMat False p prop
      let props' = Map.insert ess prop props
      atomically $ writeTVar propsVar props'
      pure (ess, prop)
    Just found -> pure (ess, found)

---------- Materialization --------------

-- Materialize property

instance
  DMat p (SMod.Property 'SMod.ValueLevel)
      (Essence, Property) where
  dMat shared p prop@(SMod.PropDict root propKVs)
    = withShared shared p root prop $ do
      let staticProp = StaticPropertyRef root
      ess         <- dMat False p root
      props       <- mapM (dMat False p) propKVs
      scriptVar   <- newTVarIO Nothing
      propBagsVar <- newTVarIO $ Map.fromList props
      propId      <- getNextPropertyId
      pure (ess,
        Property ess
          propId
          (error "owner not implemented")         -- TODO: owner
          staticProp
          scriptVar propBagsVar)
  dMat shared p prop@(SMod.PropVal root valDef)
    = withShared shared p root prop $ do
      let staticProp = StaticPropertyRef root
      ess         <- dMat False p root
      val         <- dMat False p valDef
      valVar      <- newTVarIO val
      pure (ess, ValueProperty ess staticProp valVar)

  dMat _ p (SMod.StaticProp root) = do
    error "stat prop not implemented"

  dMat _ p (SMod.StaticPropRef prop) = do
    let SMod.StaticProp root = prop
    let statEss = getEssence root

    DEnv (SEnv _ statPropsVar) propIdVar objIdVar _ <- ask
    unless (Map.member statEss statProps)
      $ error $ "Static property not found: " <> show statEss

    ess <- dMat False p statEss
    let staticProp = StaticPropertyRef root
    valVar <- newTVarIO $ StaticPropertyRefValue root
    pure (ess, ValueProperty ess staticProp valVar)

  dMat _ p (SMod.PropScript root script) = do
      let staticProp = StaticPropertyRef root
      ess         <- dMat False p root
      propBagsVar <- newTVarIO Map.empty
      scriptVar   <- newTVarIO $ Just $ Script script
      propId      <- getNextPropertyId
      pure (ess,
        Property ess
          propId
          (error "owner not implemented")
          staticProp
          scriptVar propBagsVar)

instance
  DMat p (SMod.PropertyOwning 'SMod.ValueLevel)
    (Essence, PropertyOwning) where
  dMat _ p (SMod.OwnProp prop) = do
    (ess', prop') <- dMat False p prop
    pure (ess', OwnProperty prop')
  dMat _ p (SMod.SharedProp prop) = do
    (ess', _) <- dMat True p prop
    pure (ess', SharedProperty $ DynamicPropertyRef ess')

instance
  DMat p (SMod.PropertyKeyValue 'SMod.ValueLevel)
      (Essence, PropertyBag) where
  dMat _ p (SMod.PropKeyBag ess ownings) = do
    ess'     <- dMat False p ess
    ownings' <- mapM (dMat False p) ownings
    pure (ess', PropertyDict $ Map.fromList ownings')
  dMat _ p (SMod.PropKeyVal ess owning) = do
    ess'    <- dMat False p ess
    (_, owning') <- dMat False p owning
    pure (ess', SingleProperty owning')

