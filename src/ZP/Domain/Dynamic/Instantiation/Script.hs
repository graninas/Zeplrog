{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}

module ZP.Domain.Dynamic.Instantiation.Script
  ( makeScript
  ) where

import ZP.Prelude

import ZP.Domain.Static.Model
import qualified ZP.Domain.Static.Query as SQ
import qualified ZP.Domain.Dynamic.Model as DMod
import qualified ZP.Domain.Dynamic.Instantiation.Common as DInst
import qualified ZP.Domain.Dynamic.Query as Q
import ZP.Domain.EssenceUtils

import ZP.System.TypeSelector.Granular
import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map
import qualified GHC.Types as GHC
import Unsafe.Coerce (unsafeCoerce)


data IScrRuntime = IScrRuntime
  { iScrVars :: IORef (Map.Map String (IORef GHC.Any))
  }

type ScriptInterpreter a = ReaderT IScrRuntime IO a

class IScr it a | it -> a where
  iScr :: DMod.Property -> it -> ScriptInterpreter a

instance IScr CustomScriptVL () where
  iScr prop (Script _ ops) = do
    mapM_ (iScr prop) ops

instance
  IScr EssenceVL DMod.DEssence where
  iScr _ (Ess ess) = pure ess


instance IScr ScriptOpVL () where
  iScr prop (DeclareVar varDef) = do
    IScrRuntime varsRef <- ask
    vars <- liftIO $ readIORef varsRef

    case varDef of
      GenericVar name defVal -> do
        let val = fromJust $ SQ.queryValue (RelPath []) defVal
        varRef <- liftIO $ newIORef $ unsafeCoerce val

        let vars' = Map.insert name varRef vars
        liftIO $ writeIORef varsRef vars'

  iScr prop (WriteData target source) =
    readWrite prop Nothing source target

  iScr prop (Invoke func source target) =
    readWrite prop (Just func) source target


invokeF
  :: Maybe (FuncVL typeTag1 typeTag2)
  -> GHC.Any
  -> GHC.Any
invokeF Nothing anyVal = anyVal
invokeF (Just NegateF) anyVal = let
  val :: DValue = unsafeCoerce anyVal
  in case val of
        BoolValue tn b -> unsafeCoerce $ BoolValue tn $ not b
        _ -> error $ "invokeF (Just NegateF) type mismatch: " <> show val

readWrite
  :: DMod.Property
  -> Maybe (FuncVL typeTag1 typeTag2)
  -> SourceVL tag1
  -> TargetVL tag2
  -> ReaderT IScrRuntime IO ()
readWrite prop mbF
  (FromVar (GenericVar from fromGenVal))
  (ToVar   (GenericVar to   toGenVal)) = do

    IScrRuntime varsRef <- ask
    vars <- liftIO $ readIORef varsRef

    case (Map.lookup from vars, Map.lookup to vars) of
      (Nothing, _) -> error $ show $ "readWrite (FromVar, ToVar) from var not found: " <> from
      (_, Nothing) -> error $ show $ "readWrite (FromVar, ToVar) to var not found: " <> to
      (Just fromRef, Just toRef) -> liftIO $ do
        val <- readIORef fromRef
        let val' = invokeF mbF val
        writeIORef toRef val'

readWrite prop mbF
  (FromVar (GenericVar from _))
  (ToField _ toFieldSPath) = do
    IScrRuntime varsRef <- ask
    vars <- liftIO $ readIORef varsRef

    liftIO $ case Map.lookup from vars of
      Nothing      -> error $ show $ "readWrite (FromVar, ToField) from var not found: " <> from
      Just fromRef -> do
        anyVal1 <- readIORef fromRef
        let anyVal2 = invokeF mbF anyVal1

        let toFieldDPath = toDynEssPath toFieldSPath
        toValRef <- Q.queryValueRefUnsafe toFieldDPath prop
        writeIORef toValRef $ unsafeCoerce anyVal2

readWrite prop mbF
  (FromField _ fromFieldSPath)
  (ToVar (GenericVar to _)) = do
    IScrRuntime varsRef <- ask
    vars <- liftIO $ readIORef varsRef

    case Map.lookup to vars of
      Nothing    -> error $ show $ "readWrite (FromField, ToVar) To var not found: " <> to
      Just toVarRef -> do

        let fromFieldDPath = toDynEssPath fromFieldSPath
        curValRef <- liftIO $ Q.queryValueRefUnsafe fromFieldDPath prop
        curVal    <- liftIO $ readIORef curValRef

        let anyVal2 = invokeF mbF $ unsafeCoerce curVal
        writeIORef toVarRef anyVal2

readWrite prop mbF
  (FromField _ fromFieldSPath)
  (ToField   _ toFieldSPath) = do
    let fromFieldDPath = toDynEssPath fromFieldSPath
    let toFieldDPath   = toDynEssPath toFieldSPath

    fromValRef <- liftIO $ Q.queryValueRefUnsafe fromFieldDPath  prop
    fromVal    <- liftIO $ readIORef fromValRef
    toValRef   <- liftIO $ Q.queryValueRefUnsafe toFieldDPath  prop

    let anyVal = invokeF mbF $ unsafeCoerce fromVal
    writeIORef toValRef $ unsafeCoerce anyVal

readWrite prop mbF (FromConst (GenericConst constVal))
                   (ToVar (GenericVar to _)) = do
  IScrRuntime varsRef <- ask
  vars <- readIORef varsRef

  case Map.lookup to vars of
    Nothing    -> error $ show $ "To var not found: " <> to
    Just toRef -> do
      let val = fromJust $ SQ.queryValue (RelPath []) constVal
      let anyVal = invokeF mbF $ unsafeCoerce val
      writeIORef toRef anyVal

readWrite prop mbF (FromConst (GenericConst constVal))
                   (ToField _ toFieldSPath) = do
  let toFieldDPath = toDynEssPath toFieldSPath

  toValRef <- liftIO $ Q.queryValueRefUnsafe toFieldDPath prop
  let val = fromJust $ SQ.queryValue (RelPath []) constVal
  let anyVal = invokeF mbF $ unsafeCoerce val
  writeIORef toValRef $ unsafeCoerce anyVal



makeIScrRuntime :: IO IScrRuntime
makeIScrRuntime = do
  varsRef <- newIORef Map.empty
  pure $ IScrRuntime varsRef

clearRuntime :: IScrRuntime -> IO ()
clearRuntime (IScrRuntime varsRef) = writeIORef varsRef Map.empty

makeScript
  :: DMod.Property
  -> PropertyScriptVL
  -> IO (DMod.DEssence, DMod.DynamicScript)
makeScript prop (PropScript (Ess ess) customScr) = do
  runtime <- makeIScrRuntime

  -- TODO: bracket pattern and error handling
  let ioAct = do
        putStrLn $ "Running script: " <> ess
        runReaderT (iScr prop customScr) runtime
        clearRuntime runtime
        putStrLn $ "Script finished: " <> ess
  pure (ess, DMod.DynScript ioAct)
