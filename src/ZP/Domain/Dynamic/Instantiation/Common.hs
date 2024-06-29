{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Instantiation.Common where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Dynamic.Instantiation.Instantiator
import ZP.Domain.Dynamic.Model

import Data.Proxy
import System.Random
import qualified Data.Map.Strict as Map


-- Instantiate group and essence

instance
  DInst () SMod.EssenceVL Essence where
  dInst _ () (SMod.Ess ess) = pure ess

instance DInst () [SMod.EssenceVL] EssencePath where
  dInst _ () path = mapM (dInst False ()) path

instance
  DInst () SMod.PropertyGroupVL (Essence, SMod.StaticPropertyId) where
  dInst _ () (SMod.GroupId statEss sId) = do
    ess <- dInst False () statEss
    pure (ess, sId)
  dInst _ () (SMod.GroupRootId statEss sId _) = do
    ess <- dInst False () statEss
    pure (ess, sId)



toDynEss :: SMod.EssenceVL -> Essence
toDynEss (SMod.Ess ess) = ess

toDynEssPath :: [SMod.EssenceVL] -> EssencePath
toDynEssPath esss = map toDynEss esss
