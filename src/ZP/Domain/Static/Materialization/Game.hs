{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Static.Materialization.Game where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization.Materializer
import ZP.Domain.Static.Materialization.Common
import ZP.Domain.Static.Materialization.Effect
import ZP.Domain.Static.Materialization.Property

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


data Props ps
data Triggs ts

-- Statically materialize triggers

instance
  Mat (Triggs '[]) [Trigger 'ValueLevel] where
  mat _ = pure []

instance
  ( Mat trig (Trigger 'ValueLevel)
  , Mat (Triggs triggs) [Trigger 'ValueLevel]
  ) =>
  Mat (Triggs (trig ': triggs))
      [Trigger 'ValueLevel] where
  mat _ = do
    trig  <- mat $ Proxy @trig
    triggs <- mat $ Proxy @(Triggs triggs)
    pure $ trig : triggs

-- Statically materialize props

instance
  Mat (Props '[]) [(Essence 'ValueLevel, Property 'ValueLevel)] where
  mat _ = pure []

instance
  ( Mat prop (Essence 'ValueLevel, Property 'ValueLevel)
  , Mat (Props props) [(Essence 'ValueLevel, Property 'ValueLevel)]
  ) =>
  Mat (Props (prop ': props))
      [(Essence 'ValueLevel, Property 'ValueLevel)] where
  mat _ = do
    prop  <- mat $ Proxy @prop
    props <- mat $ Proxy @(Props props)
    pure $ prop : props

-- Statically materialize game env

instance
  ( Mat (Props props) [(Essence 'ValueLevel, Property 'ValueLevel)]
  , Mat (Triggs triggs) [Trigger 'ValueLevel]
  ) =>
  Mat ('GameEnvironment @TypeLevel props triggs)
      (Game 'ValueLevel) where
  mat _ = do
    props  <- mat $ Proxy @(Props props)
    triggs <- mat $ Proxy @(Triggs triggs)
    let props' = map snd props
    pure $ GameEnvironment props' triggs
