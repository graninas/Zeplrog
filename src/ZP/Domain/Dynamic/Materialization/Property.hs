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


withShared
  :: Bool
  -> SMod.StaticPropertyRoot 'SMod.ValueLevel
  -> SMod.Property 'SMod.ValueLevel
  -> DMaterializer (Essence, Property)
  -> DMaterializer (Essence, Property)
withShared False root _ matProp = matProp
withShared True root prop matProp = do
  ess <- dMat False root
  DEnv _ propsVar <- ask
  props <- readTVarIO propsVar
  case Map.lookup ess props of
    Nothing -> do
      (_, prop) <- dMat False prop
      let props' = Map.insert ess prop props
      atomically $ writeTVar propsVar props'
      pure (ess, prop)
    Just found -> pure (ess, found)

---------- Materialization --------------

-- Materialize property

instance
  DMat (SMod.Property 'SMod.ValueLevel)
      (Essence, Property) where
  dMat shared prop@(SMod.PropDict root propKVs)
    = withShared shared root prop $ do
      let spRef = StaticPropertyRef root
      ess       <- dMat False root
      props     <- mapM (dMat False) propKVs
      scriptVar <- newTVarIO Nothing
      propsVar  <- newTVarIO $ Map.fromList props
      valVar    <- newTVarIO Nothing
      pure (ess, Property ess spRef scriptVar propsVar valVar)
  dMat shared prop@(SMod.PropConst root valDef)
    = withShared shared root prop $ do
      let spRef = StaticPropertyRef root
      ess       <- dMat False root
      val       <- dMat False valDef
      scriptVar <- newTVarIO Nothing
      valVar    <- newTVarIO $ Just $ ConstValue val
      propsVar  <- newTVarIO Map.empty
      pure (ess, Property ess spRef scriptVar propsVar valVar)
  dMat shared prop@(SMod.PropVal root valDef)
    = withShared shared root prop $ do
      let spRef = StaticPropertyRef root
      ess       <- dMat False root
      val       <- dMat False valDef
      dynValVar <- newTVarIO val
      valVar    <- newTVarIO $ Just $ VarValue dynValVar
      scriptVar <- newTVarIO Nothing
      propsVar  <- newTVarIO Map.empty
      pure (ess, Property ess spRef scriptVar propsVar valVar)

  dMat _ (SMod.StaticProp root) = do
    error "stat prop not implemented"

  dMat _ (SMod.StaticPropRef prop) = do
    let SMod.StaticProp root = prop
    let statEss = getEssence root

    DEnv (SEnv _ statPropsVar) _ <- ask
    statProps <- readTVarIO statPropsVar

    unless (Map.member statEss statProps)
      $ error $ "Static property not found: " <> show statEss

    ess <- dMat False statEss
    let spRef = StaticPropertyRef root
    propsVar  <- newTVarIO Map.empty
    scriptVar <- newTVarIO Nothing
    valVar    <- newTVarIO Nothing
    pure (ess, Property ess spRef scriptVar propsVar valVar)

  dMat _ (SMod.PropScript root script) = do
      let spRef = StaticPropertyRef root
      ess       <- dMat False root
      valVar    <- newTVarIO Nothing
      propsVar  <- newTVarIO Map.empty
      scriptVar <- newTVarIO $ Just $ Script script
      pure (ess, Property ess spRef scriptVar propsVar valVar)


getEssence
  :: SMod.StaticPropertyRoot 'SMod.ValueLevel
  -> SMod.Essence 'SMod.ValueLevel
getEssence (SMod.EssStaticRoot ess) = ess
getEssence (SMod.PropStaticRoot ess _) = ess

instance
  DMat (SMod.PropertyOwning 'SMod.ValueLevel)
    (Essence, PropertyOwning) where
  dMat _ (SMod.OwnProp prop) = do
    (ess', prop') <- dMat False prop
    pure (ess', OwnProperty prop')
  dMat _ (SMod.SharedProp prop) = do
    (ess', _) <- dMat True prop
    pure (ess', SharedProperty $ DynamicPropertyRef ess')

instance
  DMat (SMod.PropertyKeyValue 'SMod.ValueLevel)
      (Essence, PropertyBag) where
  dMat _ (SMod.PropKeyBag ess ownings) = do
    ess'     <- dMat False ess
    ownings' <- mapM (dMat False) ownings
    pure (ess', PropertyDict $ Map.fromList ownings')
  dMat _ (SMod.PropKeyVal ess owning) = do
    ess'    <- dMat False ess
    (_, owning') <- dMat False owning
    pure (ess', SingleProperty owning')

