{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZP.Domain.Hardcode.KnowledgeBase where

import ZP.Prelude
import GHC.TypeLits
import qualified Text.Show as T

import ZP.Domain.Static.Model


type EPos = Ess @TypeLevel "intrinsics:pos"

type PosVal x y   = PropVal   (EssRoot EPos) (PairValue (IntValue x) (IntValue y))
