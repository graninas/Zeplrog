{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ZP.Domain.Static.Materialization.Common where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization.Materializer

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


-- Helper to materialize the list of essences
data Essences essPath

-- Statically materialize variable def

instance
  ( KnownSymbol str
  ) =>
  SMat () str String where
  sMat () _ = pure $ symbolVal $ Proxy @str


-- Statically materialize tag property group

instance
  ( SMat () ess EssenceVL
  ) =>
  SMat () ('TagGroup @'TypeLevel ess)
          TagPropertyGroupVL where
  sMat () _ = do
    ess <- sMat () $ Proxy @ess
    pure $ TagGroup ess

instance
  ( SMat () ess EssenceVL
  , SMat () tagProp TagPropertyVL
  ) =>
  SMat () ('TagGroupRoot @'TypeLevel ess tagProp)
          TagPropertyGroupVL where
  sMat () _ = do
    ess      <- sMat () $ Proxy @ess
    tagProp  <- sMat () $ Proxy @tagProp
    pure $ TagGroupRoot ess tagProp

-- Statically materialize tag property

instance
  ( SMat () tagGroup TagPropertyGroupVL
  ) =>
  SMat () ('TagProp @'TypeLevel tagGroup)
          TagPropertyVL where
  sMat () _ = do
    tagGroup <- sMat () $ Proxy @tagGroup
    pure $ TagProp tagGroup

-- Statically materialize value

instance
  ( SMat () val (TagToType 'ValueLevel tag)
  ) =>
  SMat (Proxy tag)
        ('GenericValue @'TypeLevel @tag val)
        (GenericValDefVL tag) where
  sMat _ _ = do
    val <- sMat () $ Proxy @val
    pure $ GenericValue val

instance
  ( t ~ TagToType 'ValueLevel IntTag
  , KnownNat intVal
  ) =>
  SMat () intVal t where
  sMat () _ = pure
      $ fromIntegral
      $ natVal
      $ Proxy @intVal

instance
  ( t ~ TagToType 'ValueLevel StringTag
  , KnownSymbol s
  ) =>
  SMat () s t where
  sMat () _ = pure
      $ symbolVal
      $ Proxy @s

instance
  ( t ~ TagToType 'ValueLevel BoolTag
  ) =>
  SMat () 'True t where
  sMat () _ = pure True

instance
  ( t ~ TagToType 'ValueLevel BoolTag
  ) =>
  SMat () 'False t where
  sMat () _ = pure False

instance
  ( t ~ TagToType 'ValueLevel PairIntIntTag
  , KnownNat i1
  , KnownNat i2
  ) =>
  SMat () ('Pair i1 i2) t where
  sMat () _ = pure $
    Pair (fromIntegral $ natVal $ Proxy @i1)
         (fromIntegral $ natVal $ Proxy @i2)

-- instance
--   ( SMat () valDef ValDefVL
--   ) =>
--   SMat () ('OverriddableValue @'TypeLevel valDef) ValDefVL where
--   sMat () _ = do
--     valDef <- sMat () $ Proxy @valDef
--     pure $ OverriddableValue valDef

-- instance
--   ( SMat () (Essences essPath) [EssenceVL]
--   ) =>
--   SMat () ('PathValue @'TypeLevel essPath)
--          ValDefVL where
--   sMat () _ = do
--     path <- sMat () $ Proxy @(Essences essPath)
--     pure $ PathValue path

-- instance
--   ( SMat () tagProp TagPropertyVL
--   , SMat () valDef ValDefVL
--   ) =>
--   SMat () ('TagValue @'TypeLevel tagProp valDef)
--          ValDefVL where
--   sMat () _ = do
--     tagProp <- sMat () $ Proxy @tagProp
--     valDef  <- sMat () $ Proxy @valDef
--     pure $ TagValue tagProp valDef

-- Statically materialize Essence path

instance
  ( KnownSymbol symb
  ) =>
  SMat () ('Ess @'TypeLevel symb) EssenceVL where
  sMat () _ = pure $ Ess $ symbolVal (Proxy @symb)

instance
  SMat () (Essences '[]) [EssenceVL] where
  sMat () _ = pure []

instance
  ( SMat () ess EssenceVL
  , SMat () (Essences essPath) [EssenceVL]
  ) =>
  SMat () (Essences (ess ': essPath))
         [EssenceVL] where
  sMat () _ = do
    ess     <- sMat () $ Proxy @ess
    essPath <- sMat () $ Proxy @(Essences essPath)
    pure $ ess : essPath
