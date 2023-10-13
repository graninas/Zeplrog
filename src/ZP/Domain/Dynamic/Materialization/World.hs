{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Materialization.World where

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
import qualified Data.Vector as V


-- Materialization of World

toVector :: [String] -> Vector (Vector Char)
toVector = V.fromList . map V.fromList

toMap :: [String] -> Map.Map (Int, Int) Char
toMap wd = Map.fromList [((i, j), val) |
                        (i, row) <- zip [0..] wd,
                        (j, val) <- zip [0..] row]

instance
  DMat p SMod.WorldVL World where
  dMat _ p (SMod.WorldData rows) = do
    let v = toVector rows
    let m = toMap rows
    pure $ World v m
