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

data GameBuilder world props triggs macroses
data GameBuilder1 world props triggs macro

instance
  ( SMat world (World 'ValueLevel)
  ) =>
  SMat (GameBuilder1 world props triggs ('UseWorld world))
       (World 'ValueLevel) where
  sMat _ = do
    world <- sMat $ Proxy @world
    pure world




instance
  SMat (GameBuilder world props triggs '[])
       (World 'ValueLevel) where
  sMat _ = pure ()

instance
  ( SMat (GameBuilder1 world props triggs macro)
         (World 'ValueLevel)
  , SMat (GameBuilder world props triggs macroses)
         (World 'ValueLevel)
  ) =>
  SMat (GameBuilder world props triggs (macro ': macroses))
       (World 'ValueLevel) where
  sMat _ = do
    _ <- sMat $ Proxy @(GameBuilder1 world props triggs macro)
    _ <- sMat $ Proxy @(GameBuilder world props triggs macroses)
    pure ()

instance
  ( SMat (GameBuilder world props triggs macroses)
         (World 'ValueLevel)
  ) =>
  SMat ('MGame macroses) (Game 'ValueLevel) where
  sMat _ = do
    game <- sMat $ Proxy @(GameBuilder world props triggs macroses)

    -- tmp
    pure $ GameEnvironment @'ValueLevel (WorldData []) [] []

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

