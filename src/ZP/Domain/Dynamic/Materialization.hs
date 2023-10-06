{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module ZP.Domain.Dynamic.Materialization where

import ZP.Prelude

import ZP.Domain.Static.Model
import qualified ZP.Domain.Dynamic.Model as DMod

import GHC.TypeLits
import Data.Proxy


class Mat a b | a -> b where
  mat :: Proxy a -> IO b


instance
  KnownSymbol symb =>
  Mat ('Ess symb) String where
  mat _ = pure $ symbolVal (Proxy @symb)


instance KnownNat intVal =>
  Mat ('IntValDef intVal) Int where
  mat _ = pure $ fromIntegral $ natVal $ Proxy @intVal



-- type HPVal hp = PropVal (EssRoot EHP) (IntValDef hp)

instance
  (Mat valDef t) =>
  Mat ('PropVal root valDef) () where
  mat _ = do
    val <- mat $ Proxy @valDef
    valVar <- newTVarIO val
    pure ()

instance Mat prop () => Mat ('OwnProp prop) () where
  mat _ = do

    propVar <- newTVarIO ()
    pure ()
