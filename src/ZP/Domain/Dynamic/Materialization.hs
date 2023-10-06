{-# LANGUAGE DataKinds #-}
module ZP.Domain.Dynamic.Materialization where

import ZP.Prelude

import ZP.Domain.Static.Model

import GHC.TypeLits
import Data.Proxy


class Mat a b | a -> b where
  mat :: Proxy a -> b


instance
  KnownSymbol symb =>
  Mat ('Ess symb) String where
  mat _ = symbolVal (Proxy @symb)






materialize :: ()
materialize = ()
