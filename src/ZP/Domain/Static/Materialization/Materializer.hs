{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Materializer where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Static.Model

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


---------- Interface ------------------

-- TODO: make Materializer thread-safe.
--  Currently, TVars do not do anything useful.

type StaticProperties = Map.Map StaticPropertyId (EssenceVL, PropertyVL)
type StaticEssences   = Map.Map EssenceVL (StaticPropertyId, PropertyVL)

data SEnv = SEnv
  DebugMode
  (TVar StaticPropertyId)
  (TVar StaticProperties)
  (TVar StaticEssences)

type SMaterializer a = ReaderT SEnv IO a

-- | Materialization type class.
class SMat payload a b | a -> b where
  sMat :: payload -> Proxy a -> SMaterializer b

runSMaterializer :: SEnv -> SMaterializer a -> IO a
runSMaterializer sEnv@(SEnv dbg _ _ _) m = do
  when (dbg == DebugEnabled) $ trace "\n" $ pure ()
  runReaderT m sEnv

sMat' :: SMat payload a b => SEnv -> payload -> Proxy a -> IO b
sMat' sEnv p proxy = runSMaterializer sEnv $ sMat p proxy

makeSEnv :: DebugMode -> IO SEnv
makeSEnv dbg = SEnv
  <$> pure dbg
  <*> newTVarIO (StaticPropertyId 0)
  <*> newTVarIO Map.empty
  <*> newTVarIO Map.empty

----- Utils ---------------


getEssence :: PropertyRootVL -> EssenceVL
getEssence (EssRoot ess) = ess
getEssence (PropRoot ess _) = ess

getEssenceFromKV :: PropertyKeyValueVL -> EssenceVL
getEssenceFromKV (PropKeyBag ess _) = ess
getEssenceFromKV (PropKeyVal ess _) = ess

getRoot :: PropertyVL -> PropertyRootVL
getRoot (StaticProp root) = root
getRoot (StaticPropRef prop) = getRoot prop
getRoot (PropVal root _) = root
getRoot (PropDict root _) = root
getRoot (PropScript root _) = root


getNextStaticPropertyId'
  :: TVar StaticPropertyId
  -> SMaterializer StaticPropertyId
getNextStaticPropertyId' statPropIdVar = atomically $ do
  StaticPropertyId pId <- readTVar statPropIdVar
  writeTVar statPropIdVar $ StaticPropertyId $ pId + 1
  pure $ StaticPropertyId pId

getNextStaticPropertyId :: SMaterializer StaticPropertyId
getNextStaticPropertyId = do
  SEnv _ statPropIdVar _ _ <- ask
  getNextStaticPropertyId' statPropIdVar

addStaticProperty
  :: (StaticPropertyId, EssenceVL, PropertyVL)
  -> SMaterializer ()
addStaticProperty (statPropId, ess, prop) = do
  SEnv _ _ statPropsVar statEssencesVar <- ask
  atomically $ do
    props <- readTVar statPropsVar
    esss  <- readTVar statEssencesVar
    writeTVar statPropsVar    $ Map.insert statPropId (ess, prop) props
    writeTVar statEssencesVar $ Map.insert ess (statPropId, prop) esss


traceDebug :: String -> SMaterializer ()
traceDebug msg = do
  SEnv dbg _ _ _ <- ask
  when (dbg == DebugEnabled) $ trace msg $ pure ()
