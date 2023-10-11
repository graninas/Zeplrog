{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZP.Domain.Static.Macro where

import ZP.Prelude
import GHC.TypeLits
import qualified Text.Show as T

import ZP.Domain.Static.Model.Common
import ZP.Domain.Static.Model.Property
import ZP.Domain.Static.Model.Effect
import ZP.Domain.Static.Model.World
import ZP.Domain.Static.Model.Script


------ Macro -----

data MacroGame where
  MGame :: [Macro] -> MacroGame

data Macro where
  UseWorld    :: World TypeLevel -> Macro
  UseTriggers :: [Trigger TypeLevel] -> Macro
  Displace    :: Nat -> Nat -> Property TypeLevel -> Macro

