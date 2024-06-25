{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Instantiation.World where

import ZP.Prelude

import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.Domain.Dynamic.Model
import ZP.Domain.Dynamic.Instantiation.Instantiator
import ZP.Domain.Dynamic.Instantiation.Common
import ZP.Domain.Dynamic.Instantiation.Property
import ZP.Domain.Dynamic.Instantiation.Effect

import Data.Proxy
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V


-- Instatiation of World

worldDataToVector :: [String] -> Vector (Vector Char)
worldDataToVector = V.fromList . map V.fromList

worldDataToList :: [String] -> [ ((Int, Int), Char) ]
worldDataToList wd = [ ((i, j), val)
                     | (i, row) <- zip [0..] wd
                     , (j, val) <- zip [0..] row
                     ]

worldDimensions :: [String] -> (Int, Int)
worldDimensions [] = (0, 0)
worldDimensions rs@(r:_) = (length rs, length r)

instance
  DInst () SMod.WorldVL World where
  dInst _ () (SMod.WorldData rows) = do
    let dims = worldDimensions rows
    pure $ World dims $ worldDataToVector rows
