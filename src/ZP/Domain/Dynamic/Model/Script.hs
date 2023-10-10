{-# LANGUAGE DataKinds #-}

module ZP.Domain.Dynamic.Model.Script where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import ZP.Domain.Dynamic.Model.Common

-- TODO: convert to a true dynamic model for performance optimization
newtype Script = Script (SMod.Script 'SMod.ValueLevel)
