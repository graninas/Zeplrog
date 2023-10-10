{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Script where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization.Materializer
import ZP.Domain.Static.Materialization.Common

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


---------- Materialization --------------

