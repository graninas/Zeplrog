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

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map



------ Stat materialization macro -----

data Macroses macroses

type G = Game 'ValueLevel

-- Statically interpret macro

instance
  ( SMat () world (World 'ValueLevel)
  ) =>
  SMat G ('UseWorld world) G where
  sMat (GameEnvironment _ props triggs) _ = do
    world <- sMat () $ Proxy @world
    pure $ GameEnvironment world props triggs

instance
  -- ( SMat () (Triggs triggs) [Trigger 'ValueLevel]
  -- ) =>
  SMat G ('UseTriggers world) G where
  sMat (GameEnvironment world props triggs) _ = do
    -- triggs <- sMat () $ Proxy @(Triggs triggs)
    pure $ GameEnvironment world props triggs

instance
  SMat G ('Displace a b c) G where
  sMat (GameEnvironment world props triggs) _ = do
    pure $ GameEnvironment world props triggs

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
    let emptyGame = GameEnvironment @'ValueLevel (WorldData []) [] []
    sMat emptyGame $ Proxy @(Macroses macroses)


-- data Game (lvl :: Level) where
--   GameEnvironment
--     :: World lvl
--     -> [ Property lvl ]
--     -> [ Trigger lvl ]
--     -> Game lvl


-- data MacroGame where
--   MGame :: [Macro] -> MacroGame

-- data Macro where
--   UseWorld    :: World TypeLevel -> Macro
--   UseTriggers :: [Trigger TypeLevel] -> Macro
--   Displace    :: Nat -> Nat -> Property TypeLevel -> Macro

