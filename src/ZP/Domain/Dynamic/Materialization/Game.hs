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
import ZP.Domain.Dynamic.Materialization.World

import Data.Proxy
import qualified Data.Map.Strict as Map


-- Materialization of Game

instance
  DMat p SMod.GameVL Game where
  dMat _ p (SMod.GameEnvironment world cells statProps triggs) = do
    DEnv _ propIdVar objIdVar dynPropsVar
    let statProps = Map.fromList
          [(getEssence $ getRoot sProp, sProp) | sProp <- statProps]

    let props = error "props not implemented"
    let activeObjs = error "objects not implemented"

    world'  <- dMat False p world
    let propDict = Map.fromList props'

    -- TODO: triggs
    triggs' <- mapM (dMat False p) triggs
    -- TODO: cells
    -- pure $ Game world' (error "cells not implemented") propDict triggs'

    pure $ Game
      world'
      propIdVar
      objIdVar
      statProps
      props
      activeObjs
      triggs
