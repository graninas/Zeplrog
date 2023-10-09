{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Materialization where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materializer as SMat
import ZP.Domain.Dynamic.Model

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map as Map


---------- Interface ------------------

data Env = Env
  SMat.StaticProperties
  (TVar (Map.Map Essence Property))

type Materializer a = ReaderT Env IO a

type Shared = Bool

-- | Materialization type class.
class Mat a b | a -> b where
  mat :: Shared -> a -> Materializer b

runMaterializer
  :: SMat.StaticProperties
  -> Materializer a
  -> IO (Env, a)
runMaterializer staticProps m = do
  sharedProps <- liftIO $ newTVarIO Map.empty
  let env = Env staticProps sharedProps
  res <- runReaderT m env
  pure (env, res)

mat' :: Mat a b => SMat.StaticProperties -> a -> IO b
mat' staticProps statModel = do
  (_, a) <- runMaterializer staticProps (mat False statModel)
  pure a

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

-- Materialize root and essence

instance
  Mat (SMod.Essence 'SMod.ValueLevel) Essence where
  mat _ (SMod.Ess ess) = pure ess

instance
  Mat (SMod.StaticPropertyRoot 'SMod.ValueLevel) Essence where
  mat _ (SMod.EssStaticRoot ess) = mat False ess

-- Materialize value

instance
  Mat (SMod.ValDef 'SMod.ValueLevel) Value where
  mat _ (SMod.IntValue val) = pure $ IntValue val
  mat _ (SMod.PairValue val1 val2) = do
    val1' <- mat False val1
    val2' <- mat False val2
    pure $ PairValue val1' val2'
  mat _ _ = error "mat valdef not implemented"


-- Materialize property

instance
  Mat (SMod.Property 'SMod.ValueLevel)
      (Essence, Property) where
  mat shared prop@(SMod.PropDict root propKVs)
    = withShared shared root prop $ do
      let spRef = StaticPropertyRef root
      ess      <- mat False root
      props    <- mapM (mat False) propKVs
      propsVar <- newTVarIO $ Map.fromList props
      valVar   <- newTVarIO Nothing
      pure (ess, Property ess spRef propsVar valVar)
  mat shared prop@(SMod.PropConst root valDef)
    = withShared shared root prop $ do
      let spRef = StaticPropertyRef root
      ess      <- mat False root
      val      <- mat False valDef
      valVar   <- newTVarIO $ Just $ ConstValue val
      propsVar <- newTVarIO $ Map.empty
      pure (ess, Property ess spRef propsVar valVar)
  mat shared prop@(SMod.PropVal root valDef)
    = withShared shared root prop $ do
      let spRef = StaticPropertyRef root
      ess       <- mat False root
      val       <- mat False valDef
      dynValVar <- newTVarIO $ val
      valVar    <- newTVarIO $ Just $ VarValue dynValVar
      propsVar  <- newTVarIO $ Map.empty
      pure (ess, Property ess spRef propsVar valVar)
  mat _ (SMod.StaticPropRef statProp) = do
    let statEss = getStaticEssence statProp
    let statRoot = getStaticPropRoot statProp
    ess <- mat False statEss

    Env statProps _ <- ask
    unless (Map.member statEss statProps)
      $ error $ "Static property not found: " <> show ess

    let ref = StaticPropertyRef statRoot
    propsVar <- newTVarIO Map.empty
    valVar <- newTVarIO Nothing
    pure (ess, Property ess ref propsVar valVar)

getStaticEssence
  :: SMod.StaticProperty 'SMod.ValueLevel
  -> SMod.Essence 'SMod.ValueLevel
getStaticEssence (SMod.StaticProp (SMod.EssStaticRoot ess)) = ess
getStaticEssence (SMod.StaticProp (SMod.PropStaticRoot ess _)) = ess

getStaticPropRoot
  :: SMod.StaticProperty 'SMod.ValueLevel
  -> SMod.StaticPropertyRoot 'SMod.ValueLevel
getStaticPropRoot (SMod.StaticProp root) = root

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

