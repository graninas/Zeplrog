{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ZP.Domain.Static.Materialization.Common where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization.Materializer

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


-- Helper to materialize the list of essences
data Essences essPath

-- Statically materialize elementary types

instance
  ( t ~ TagToType 'ValueLevel IntTag
  , KnownNat intVal
  ) =>
  SMat () intVal t where
  sMat () _ = pure
      $ fromIntegral
      $ natVal
      $ Proxy @intVal

instance SMat () 'True Bool where
  sMat () _ = pure True

instance SMat () 'False Bool where
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

-- Statically materialize Essence & path

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


-- Statically materialize generic value

instance
  ( SMat () tagProp TagPropertyVL
  , SMat (Proxy childTag) genVal (GenericValDefVL childTag)
  ) =>
  SMat (Proxy childTag)
       ('TVH tagProp genVal)
       (TagValueHolderVL childTag) where
  sMat _ parentGenVal = do
    tagProp <- sMat () $ Proxy @tagProp
    genVal  <- sMat (Proxy @childTag) $ Proxy @genVal
    pure $ TVH tagProp genVal



instance
  ( SMat (Proxy childTag) tvh (TagValueHolderVL childTag)
  ) =>
  SMat (Proxy (TVHTag childTag))
       ('GenericValue tvh 'DPlaceholder)
       (GenericValDefVL (TVHTag childTag)) where
  sMat _ parentGenVal = do
    tvh <- sMat (Proxy @childTag) (Proxy @tvh)
    pure (GenericValue tvh DPlaceholder)


instance
  ( KnownSymbol s
  ) =>
  SMat (Proxy "string") s (String, DValue) where
  sMat _ _ = do
    let s = symbolVal $ Proxy @s
    pure (s, StringValue "string" s)

instance
  ( SMat () (Essences essPath) [EssenceVL]
  ) =>
  SMat (Proxy "path") essPath ([EssenceVL], DValue) where
  sMat _ _ = do
    essPath <- sMat () $ Proxy @(Essences essPath)
    let dEssPath = map (\(Ess s) -> s) essPath
    pure (essPath, PathValue "path" dEssPath)

instance
  ( KnownNat n1
  , KnownNat n2
  ) =>
  SMat (Proxy "int pair") ('Pair n1 n2) (CustomPair Int Int, DValue) where
  sMat _ _ = do
    let n1 = fromIntegral $ natVal $ Proxy @n1
    let n2 = fromIntegral $ natVal $ Proxy @n2
    pure (Pair n1 n2, PairValue "int pair" (IntValue "int" n1) (IntValue "int" n2))

-- Generic value itself
instance
  ( SMat (Proxy strTag) val
    (TagToType 'ValueLevel ('RegularTag strTag), DValue)
  ) =>
  SMat
    (Proxy ('RegularTag strTag))
    ('GenericValue val 'DPlaceholder)
    (GenericValDefVL ('RegularTag strTag)) where
  sMat proxy _ = do
    (val, dVal) <- sMat (Proxy @strTag) $ Proxy @val
    pure $ GenericValue val dVal

instance
  ( SMat (Proxy "int pair") val
    (TagToType 'ValueLevel ('CompoundTag "int pair" IntTag IntTag), DValue)
  ) =>
  SMat
    (Proxy ('CompoundTag "int pair" IntTag IntTag))
    ('GenericValue val 'DPlaceholder)
    (GenericValDefVL ('CompoundTag "int pair" IntTag IntTag)) where

  sMat _ _ = do
    (genPVal, dVal) <- sMat (Proxy @"int pair") $ Proxy @val
    pure $ GenericValue genPVal dVal
