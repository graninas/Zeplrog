{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZP.Domain.Static.Model.Object where

import ZP.Prelude
import GHC.TypeLits
import qualified Text.Show as T

import ZP.Domain.Static.Model.Common
import ZP.Domain.Static.Model.Property
import ZP.Domain.Static.Model.Effect

------ Object -----

-- | Objects in the world.
-- No static type-level version.
data Object (lvl :: Level) where
  Obj
    :: IntegerType lvl
    -> IntegerType lvl
    -> Property lvl
    -> Object lvl

type ObjectTL = Object 'TypeLevel
type ObjectVL = Object 'ValueLevel

