{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZP.Domain.Static.Transform where

import ZP.Prelude
import GHC.TypeLits
import qualified Text.Show as T

import ZP.Domain.Static.Model.Common
import ZP.Domain.Static.Model.Property
import ZP.Domain.Static.Model.Effect
import ZP.Domain.Static.Model.World
import ZP.Domain.Static.Model.Script



addOwnProperty'
  :: (EssenceVL, PropertyVL)
  -> PropertyVL
  -> PropertyVL
addOwnProperty' (ess, childProp) (PropDict root propKVs) = let
  propKV = PropKeyVal ess (OwnProp childProp)
  in PropDict root (propKV : propKVs)
addOwnProperty' _ _ = error "Invalid parent property (not a dict)"

addOwnProperty
  :: [EssenceVL]
  -> (EssenceVL, PropertyVL)
  -> PropertyVL
  -> PropertyVL
addOwnProperty [] childProp prop = addOwnProperty' childProp prop
addOwnProperty _ childProp prop =
  -- TODO
  error "adding prop into a bag not implemented"




addSharedProperty'
  :: (EssenceVL, PropertyVL)
  -> PropertyVL
  -> PropertyVL
addSharedProperty' (ess, childProp) (PropDict root propKVs) = let
  propKV = PropKeyVal ess (SharedProp childProp)
  in PropDict root (propKV : propKVs)
addSharedProperty' _ _ = error "Invalid parent property (not a dict)"

addSharedProperty
  :: [EssenceVL]
  -> (EssenceVL, PropertyVL)
  -> PropertyVL
  -> PropertyVL
addSharedProperty [] childProp prop = addSharedProperty' childProp prop
addSharedProperty _ childProp prop =
  -- TODO
  error "adding prop into a bag not implemented"
