module ZP.Domain.Dynamic.Interaction where

import ZP.Prelude

import ZP.Domain.Dynamic.Model
import qualified ZP.Domain.Dynamic.Query as Q

import qualified Data.Map as Map

invoke
  :: DEssence
  -> Property
  -> IO ()
invoke ess (Prop _ _ _ _ _ scripts) = do
  case Map.lookup ess scripts of
    Nothing -> error $ "Script not found: " <> show ess
    Just (DynScript act) -> act


