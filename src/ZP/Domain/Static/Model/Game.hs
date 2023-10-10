{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZP.Domain.Static.Model.Game where

import ZP.Prelude
import GHC.TypeLits
import qualified Text.Show as T

import ZP.Domain.Static.Model.Common
import ZP.Domain.Static.Model.Property
import ZP.Domain.Static.Model.Effect

------ Game and environment -----

data Game (lvl :: Level) where
  GameEnvironment
    :: [ Property lvl ]
    -> [ Trigger lvl ]
    -> Game lvl
