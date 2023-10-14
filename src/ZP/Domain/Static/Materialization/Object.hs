{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Object where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization.Materializer
import ZP.Domain.Static.Materialization.Common
import ZP.Domain.Static.Materialization.Effect
import ZP.Domain.Static.Materialization.Property
import ZP.Domain.Static.Materialization.World

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


data Objs ts


-- Statically materialize object

instance
  ( KnownNat x
  , KnownNat y
  , SMat p prop PropertyVL
  ) =>
  SMat p ('Obj @TypeLevel x y prop)
         ObjectVL where
  sMat p _ = do
    prop <- sMat p $ Proxy @prop
    let x = fromIntegral $ natVal $ Proxy @x
    let y = fromIntegral $ natVal $ Proxy @y
    pure $ Obj x y prop

-- Statically materialize objects

instance
  SMat p (Objs '[]) [ObjectVL] where
  sMat p _ = pure []

instance
  ( SMat p obj ObjectVL
  , SMat p (Objs objs) [ObjectVL]
  ) =>
  SMat p (Objs (obj ': objs))
         [ObjectVL] where
  sMat p _ = do
    obj  <- sMat p $ Proxy @obj
    objs <- sMat p $ Proxy @(Objs objs)
    pure $ obj : objs
