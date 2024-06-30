{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module ZP.Domain.EssenceUtils where

import ZP.Prelude

import ZP.Domain.Dynamic.Model
import qualified ZP.Domain.Static.Model as SMod
import qualified ZP.Domain.Static.Materialization as SMat
import ZP.System.TypeSelector.Granular

import Data.Proxy
import qualified Data.Map.Strict as Map
import GHC.TypeLits


toDynEss :: SMod.EssenceVL -> DEssence
toDynEss (SMod.Ess ess) = ess

toDynEssPath :: SMod.EssencePathVL -> DEssencePath
toDynEssPath (SMod.AbsPath path) = let
  dPath = map toDynEss path
  in DAbsPath dPath
toDynEssPath (SMod.RelPath path) = let
  dPath = map toDynEss path
  in DRelPath dPath

matEss
  :: forall ess symb
   . KnownSymbol symb
  => (ess ~ 'SMod.Ess @'TypeLevel symb)
  => DEssence
matEss = symbolVal $ Proxy @symb

matPath
  :: forall path p
   . (path ~ (p :: SMod.EssencePathTL))
  => MatPath' path
  => DEssencePath
matPath = toDynEssPath $ matPath' $ Proxy @path


--- Helper classes, pure materialization
class MatEss' ess where
  matEss' :: Proxy ess -> SMod.EssenceVL

instance
  ( KnownSymbol symb
  ) =>
  MatEss' ('SMod.Ess @'TypeLevel symb) where
  matEss' _ = SMod.Ess $ symbolVal $ Proxy @symb

class MatEsss' path where
  matEsss' :: Proxy path -> [SMod.EssenceVL]

instance
  ( MatEss' ess
  , MatEsss' (SMat.Essences path)
  ) =>
  MatEsss' (SMat.Essences (ess ': path)) where
  matEsss' _ = let
    ess = matEss' $ Proxy @ess
    in ess : matEsss' (Proxy @(SMat.Essences path))

instance
  MatEsss' (SMat.Essences '[]) where
  matEsss' _ = []


class MatPath' (path :: SMod.EssencePathTL) where
  matPath' :: Proxy path -> SMod.EssencePathVL

instance
  ( MatEsss' (SMat.Essences path)
  ) =>
  MatPath' ('SMod.AbsPath @'TypeLevel path) where
  matPath' _ = let
    path = matEsss' $ Proxy @(SMat.Essences path)
    in SMod.AbsPath path

instance
  ( MatEsss' (SMat.Essences path)
  ) =>
  MatPath' ('SMod.RelPath @'TypeLevel path) where
  matPath' _ = let
    path = matEsss' $ Proxy @(SMat.Essences path)
    in SMod.RelPath path

