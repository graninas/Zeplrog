{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ZP.Domain.Static.Materialization.Macro where

import ZP.Prelude

import ZP.System.Debug
import ZP.Domain.Static.Model
import ZP.Domain.Static.Macro
import ZP.Domain.Static.Materialization.Materializer
import ZP.Domain.Static.Materialization.Common
import ZP.Domain.Static.Materialization.Script
import ZP.Domain.Static.Materialization.Property
import ZP.Domain.Static.Materialization.Effect
import ZP.Domain.Static.Materialization.Game
import ZP.Domain.Static.Materialization.World
import ZP.Domain.Hardcode.KnowledgeBase
import ZP.Domain.Static.Transform

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map



------ Stat materialization macro -----

data Macroses macroses

type G = Game 'ValueLevel

-- Statically interpret macro

findPropertyByChar :: Char -> Map.Map Char PropertyVL -> [PropertyVL]
findPropertyByChar c objMap = case Map.lookup c objMap of
  Nothing -> []
  Just z -> [z]

propIcon :: PropertyVL -> Char
propIcon _ = '#'

-- | Using world data and props to build
-- world objects in cells
instance
  ( SMat () world WorldVL
  , SMat () (Props props) [(EssenceVL, PropertyVL)]
  ) =>
  SMat G ('UseWorld world props) G where
    -- TODO: preserve prev props
  sMat (GameEnvironment _ cellsOld propsOld triggs) _ = do
    world@(WorldData worldData) <- sMat () $ Proxy @world
    props <- sMat () $ Proxy @(Props props)

    let propsMap = Map.fromList
          [(propIcon prop, prop) | (_, prop) <- props]

    let cells = [ CellObject pos p |
          (rowIndex, row) <- zip [0..] worldData,
          (colIndex, ch)  <- zip [0..] row,
          let pos = (rowIndex, colIndex),
          p <- findPropertyByChar ch propsMap]

    pure $ GameEnvironment world (cellsOld <> cells) propsOld triggs

instance
  ( SMat () (Triggs triggs) [TriggerVL]
  ) =>
  -- TODO: preserve prev triggs
  SMat G ('UseTriggers triggs) G where
  sMat (GameEnvironment world cells props _) _ = do
    triggs <- sMat () $ Proxy @(Triggs triggs)
    pure $ GameEnvironment world cells props triggs


-- TODO: rework. Placing should go to Cells
instance
  ( KnownNat x
  , KnownNat y
  , SMat () prop (EssenceVL, PropertyVL)
  ) =>
  SMat G ('PlaceObj x y prop) G where
  sMat (GameEnvironment world cells props triggs) _ = do
    (ess, prop) <- sMat () $ Proxy @prop

    posProp <- sMat () $ Proxy @(PosVal x y)
    let prop' = addSharedProperty [] posProp prop

    -- TODO: verify the bounds

    pure $ GameEnvironment world cells (prop' : props) triggs

-- Statically interpret macroses

instance
  SMat G (Macroses '[]) G where
  sMat g _ = pure g

instance
  ( SMat G macro G
  , SMat G (Macroses macroses) G
  ) =>
  SMat G (Macroses (macro ': macroses)) G where
  sMat g1 _ = do
    g2 <- sMat g1 $ Proxy @macro
    sMat g2 $ Proxy @(Macroses macroses)

-- Statically build the game with macroses

instance
  ( SMat G (Macroses macroses) G
  ) =>
  SMat () ('MGame macroses) G where
  sMat _ _ = do
    let emptyGame = GameEnvironment @'ValueLevel (WorldData []) [] [] []
    sMat emptyGame $ Proxy @(Macroses macroses)

