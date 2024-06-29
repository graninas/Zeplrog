{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Instantiation.Object where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Static.Materialization ()
import ZP.Domain.Static.Materialization.Materializer
import qualified ZP.Domain.Static.Query as SQuery
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Instantiation.Instantiator
import ZP.Domain.Dynamic.Instantiation.Common
import ZP.Domain.Dynamic.Instantiation.Property
import ZP.Domain.Dynamic.Instantiation.Effect
import ZP.Domain.Dynamic.Query

import Data.Proxy
import qualified Data.Map.Strict as Map
import Data.Maybe


-- Instatiation of Object with positions

instance
  DInst SMod.PosEssencePathVL SMod.ObjectVL Object where
  dInst _ (SMod.PosPath pathToPos) (SMod.Obj x y sProp) = do
    essPath <- mapM (dInst False ()) pathToPos
    prop <- instProperty sProp

    let posVal = mkIntPairValue x y

    liftIO $ updateValue prop essPath posVal

    spawnObject prop

