{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Materialization.Object where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Static.Materialization ()
import ZP.Domain.Static.Materialization.Materializer
import qualified ZP.Domain.Static.Query as SQuery
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Materialization.Materializer
import ZP.Domain.Dynamic.Materialization.Common
import ZP.Domain.Dynamic.Materialization.Property
import ZP.Domain.Dynamic.Materialization.Effect
import qualified ZP.Domain.Hardcode.KnowledgeBase as KB

import Data.Proxy
import qualified Data.Map.Strict as Map
import Data.Maybe


-- Materialization of Object

instance
  DMat [SMod.EssenceVL] SMod.ObjectVL Object where
  dMat _ pathToPos (SMod.Obj x y statProp) = do
    essPath <- dMat False () pathToPos
    prop    <- dMat False () statProp

    statPosProp <- withSMaterializer
      $ sMat (x, y)
      $ Proxy @KB.DerivedPosVal
    posProp <- dMat False () statPosProp

    -- case posProp of
    --   ValueProperty _ _ valVar ->
    --     atomically $ writeTVar valVar $ PairValue (x, y)
    --   _ -> error $ "Invalid kind of property materialized instead of value."

    addChildProperty essPath posProp prop

    spawnObject prop



addChildProperty essPath posProp prop = error ""


