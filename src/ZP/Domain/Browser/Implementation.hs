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
  Browse GetEssence ('Ess ess) DMod.Essence where
  browse _ _ = symbolVal $ Proxy @ess

instance
  Browse GetEssence ess DMod.Essence =>
  Browse GetEssence ('EssRoot ess) DMod.Essence where
  browse _ _ = browse GetEssence $ Proxy @ess

instance
  Browse GetEssence root DMod.Essence =>
  Browse GetEssence ('PropConst root v) DMod.Essence where
  browse _ _ = browse GetEssence $ Proxy @root


instance
  Browse GetEssence root DMod.Essence =>
  Browse GetEssence ('PropVal root val) DMod.Essence where
  browse _ _ = browse GetEssence $ Proxy @root

instance
  Browse GetEssence root DMod.Essence =>
  Browse GetEssence ('PropDict root d) DMod.Essence where
  browse _ _ = browse GetEssence $ Proxy @root

instance
  BrowseDyn GetEssence DMod.StaticPropertyRef DMod.Essence where
  browseDyn _ (DMod.StaticPropRef proxy) = browse GetEssence proxy
