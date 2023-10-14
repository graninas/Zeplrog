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
  , SMat () prop PropertyVL
  ) =>
  SMat () ('Obj @TypeLevel x y prop)
         ObjectVL where
  sMat () _ = do
    prop <- sMat () $ Proxy @prop
    let x = fromIntegral $ natVal $ Proxy @x
    let y = fromIntegral $ natVal $ Proxy @y
    pure $ Obj x y prop

-- Statically materialize objects

instance
  SMat () (Objs '[]) [ObjectVL] where
  sMat () _ = pure []

instance
  ( SMat () obj ObjectVL
  , SMat () (Objs objs) [ObjectVL]
  ) =>
  SMat () (Objs (obj ': objs))
         [ObjectVL] where
  sMat () _ = do
    obj  <- sMat () $ Proxy @obj
    objs <- sMat () $ Proxy @(Objs objs)
    pure $ obj : objs
