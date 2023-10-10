{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Materialization.Property where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
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
  -> Materializer (Essence, Property)
  -> Materializer (Essence, Property)
withShared False root _ matProp = matProp
withShared True root prop matProp = do
  ess <- mat False root
  Env _ propsVar <- ask
  props <- readTVarIO propsVar
  case Map.lookup ess props of
    Nothing -> do
      (_, prop) <- mat False prop
      let props' = Map.insert ess prop props
      atomically $ writeTVar propsVar props'
      pure (ess, prop)
    Just found -> pure (ess, found)

---------- Materialization --------------

-- Materialize property

instance
  Mat (SMod.Property 'SMod.ValueLevel)
      (Essence, Property) where
  mat shared prop@(SMod.PropDict root propKVs)
    = withShared shared root prop $ do
      let spRef = StaticPropertyRef root
      ess      <- mat False root
      props    <- mapM (mat False) propKVs
      scriptVar <- newTVarIO Nothing
      propsVar <- newTVarIO $ Map.fromList props
      valVar   <- newTVarIO Nothing
      pure (ess, Property ess spRef scriptVar propsVar valVar)
  mat shared prop@(SMod.PropConst root valDef)
    = withShared shared root prop $ do
      let spRef = StaticPropertyRef root
      ess      <- mat False root
      val      <- mat False valDef
      scriptVar <- newTVarIO Nothing
      valVar   <- newTVarIO $ Just $ ConstValue val
      propsVar <- newTVarIO Map.empty
      pure (ess, Property ess spRef scriptVar propsVar valVar)
  mat shared prop@(SMod.PropVal root valDef)
    = withShared shared root prop $ do
      let spRef = StaticPropertyRef root
      ess       <- mat False root
      val       <- mat False valDef
      dynValVar <- newTVarIO val
      valVar    <- newTVarIO $ Just $ VarValue dynValVar
      scriptVar <- newTVarIO Nothing
      propsVar  <- newTVarIO Map.empty
      pure (ess, Property ess spRef scriptVar propsVar valVar)

  mat _ (SMod.StaticProp root) = do
    error "stat prop not implemented"

  mat _ (SMod.StaticPropRef prop) = do
    let SMod.StaticProp root = prop
    let statEss = getEssence root

    Env statProps _ <- ask
    unless (Map.member statEss statProps)
      $ error $ "Static property not found: " <> show statEss

    ess <- mat False statEss
    let spRef = StaticPropertyRef root
    propsVar  <- newTVarIO Map.empty
    scriptVar <- newTVarIO Nothing
    valVar    <- newTVarIO Nothing
    pure (ess, Property ess spRef scriptVar propsVar valVar)

  mat _ (SMod.PropScript root script) = do
      let spRef = StaticPropertyRef root
      ess       <- mat False root
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
  Mat (SMod.PropertyOwning 'SMod.ValueLevel)
    (Essence, PropertyOwning) where
  mat _ (SMod.OwnProp prop) = do
    (ess', prop') <- mat False prop
    pure (ess', OwnProperty prop')
  mat _ (SMod.SharedProp prop) = do
    (ess', _) <- mat True prop
    pure (ess', SharedProperty $ DynamicPropertyRef ess')

instance
  Mat (SMod.PropertyKeyValue 'SMod.ValueLevel)
      (Essence, PropertyBag) where
  mat _ (SMod.PropKeyBag ess ownings) = do
    ess'     <- mat False ess
    ownings' <- mapM (mat False) ownings
    pure (ess', PropertyDict $ Map.fromList ownings')
  mat _ (SMod.PropKeyVal ess owning) = do
    ess'    <- mat False ess
    (_, owning') <- mat False owning
    pure (ess', SingleProperty owning')

