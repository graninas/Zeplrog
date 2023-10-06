{-# LANGUAGE DataKinds #-}

module ZP.Domain.Browser.Implementation where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Browser.Language
import ZP.Domain.Browser.Methods
import qualified ZP.Domain.Dynamic.Model.Common as DMod
import qualified ZP.Domain.Dynamic.Model.Property as DMod

import Data.Proxy
import GHC.TypeLits


---- Browsing methods ------------------

instance
  KnownSymbol ess =>
  Browse GetEssence ('Ess ess) DMod.DynEssence where
  browse _ _ = symbolVal $ Proxy @ess

instance
  Browse GetEssence ess DMod.DynEssence =>
  Browse GetEssence ('EssRoot ess) DMod.DynEssence where
  browse _ _ = browse GetEssence $ Proxy @ess

instance
  Browse GetEssence root DMod.DynEssence =>
  Browse GetEssence ('PropConst root v) DMod.DynEssence where
  browse _ _ = browse GetEssence $ Proxy @root


instance
  Browse GetEssence root DMod.DynEssence =>
  Browse GetEssence ('PropVal root val) DMod.DynEssence where
  browse _ _ = browse GetEssence $ Proxy @root

instance
  Browse GetEssence root DMod.DynEssence =>
  Browse GetEssence ('PropDict root d) DMod.DynEssence where
  browse _ _ = browse GetEssence $ Proxy @root

instance
  BrowseDyn GetEssence DMod.StaticPropertyRef DMod.DynEssence where
  browseDyn _ (DMod.StaticPropRef proxy) = browse GetEssence proxy
