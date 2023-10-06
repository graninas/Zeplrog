{-# LANGUAGE DataKinds #-}

module ZP.Domain.Browser.Language where

import ZP.Prelude

import Data.Proxy

class Browse tag a b | tag a -> b where
  browse :: tag -> Proxy a -> b

class BrowseDyn tag dyn b | tag dyn -> b where
  browseDyn :: tag -> dyn -> b
