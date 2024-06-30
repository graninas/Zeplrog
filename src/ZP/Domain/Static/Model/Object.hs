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
data Object (lvl :: Level) where
  Obj
    :: IntegerType lvl  -- ^ Pos x
    -> IntegerType lvl  -- ^ Pos y
    -> Property lvl
    -> Object lvl

type ObjectTL = Object 'TypeLevel
type ObjectVL = Object 'ValueLevel

