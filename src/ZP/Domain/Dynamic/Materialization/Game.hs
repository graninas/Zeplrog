{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Materialization.Game where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Materialization.Materializer
import ZP.Domain.Dynamic.Materialization.Common
import ZP.Domain.Dynamic.Materialization.Property
import ZP.Domain.Dynamic.Materialization.Effect

import Data.Proxy
import qualified Data.Map.Strict as Map


-- Materialization of Game

instance
  DMat (SMod.Game 'SMod.ValueLevel) Game where
  dMat _ prop@(SMod.GameEnvironment props triggs) = do
    props'  <- mapM (dMat False) props
    triggs' <- mapM (dMat False) triggs
    let propDict = Map.fromList props'
    pure $ Game propDict triggs'