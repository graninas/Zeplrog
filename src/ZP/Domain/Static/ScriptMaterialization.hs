{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.ScriptMaterialization where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materializer

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


---------- Materialization --------------

