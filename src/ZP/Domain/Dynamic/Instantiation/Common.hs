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
  DInst () SMod.EssenceVL DEssence where
  dInst _ () (SMod.Ess ess) = pure ess

instance DInst () SMod.EssencePathVL DEssencePath where
  dInst _ () (SMod.AbsPath path) = do
    dPath <- mapM (dInst False ()) path
    pure $ DAbsPath dPath
  dInst _ () (SMod.RelPath path) = do
    dPath <- mapM (dInst False ()) path
    pure $ DRelPath dPath

instance
  DInst () SMod.PropertyGroupVL (DEssence, SMod.StaticPropertyId) where
  dInst _ () (SMod.GroupId statEss sId) = do
    ess <- dInst False () statEss
    pure (ess, sId)
  dInst _ () (SMod.GroupRootId statEss sId _) = do
    ess <- dInst False () statEss
    pure (ess, sId)



toDynEss :: SMod.EssenceVL -> DEssence
toDynEss (SMod.Ess ess) = ess

toDynEssPath :: SMod.EssencePathVL -> DEssencePath
toDynEssPath (SMod.AbsPath path) = let
    dPath = map toDynEss path
    in DAbsPath dPath
toDynEssPath (SMod.RelPath path) = let
    dPath = map toDynEss path
    in DRelPath dPath
