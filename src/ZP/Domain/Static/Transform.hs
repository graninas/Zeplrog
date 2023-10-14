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


-- -- | Adds own property into an internal dictionary.
-- addOwnProperty'
--   :: (EssenceVL, PropertyVL)
--   -> PropertyVL
--   -> PropertyVL
-- addOwnProperty' (ess, childProp) (PropDict root propKVs) = let
--   propKV = PropKeyVal ess (OwnProp childProp)
--   in PropDict root (propKV : propKVs)
-- addOwnProperty' _ _ = error "Invalid parent property (not a dict)"


-- -- TODO: follow references

-- addOwnTopLevelProperty
--   :: (EssenceVL, PropertyVL)
--   -> PropertyVL
--   -> PropertyVL
-- addOwnTopLevelProperty (childEss, childProp) prop = case prop of
--   StaticProp _  -> error $ "Can't add prop into StaticProp" <> show childEss
--   StaticPropRef prop -> addOwnProperty childProp prop
--   PropVal _ _ -> error $ "Can't add prop into PropVal: " <> show childEss
--   PropDict root kvs -> let
--     foundKVs = [ kv | kv <- kvs
--                , childEss == getEssenceFromKV kv
--                ]
--     k = PropKeyVal childEss (OwnProp childProp)
--     in case foundKVs of
--       [] -> PropDict root $ k : kvs
--       (PropKeyVal _:_) -> error $ "Can't add prop: already exists: " <> show childEss
--       (PropKeyBag _ bag:kvs) ->
--         ----------------------------------------

--   DerivedProp
--     :: Essence lvl
--     -> Property lvl
--     -> [PropertyKeyValue lvl]
--     -> Property lvl
--   -- | Property script.
--   PropScript
--     :: PropertyRoot lvl
--     -> Script lvl
--     -> Property lvl






-- -- | Adds own property into a property by path.
-- -- If the path is empty, the prop will be top-level.
-- -- If the path is not empty, it should point to the category.
-- -- Doesn't follow references to props in the form of essence paths.
-- -- Fails if the property already exists.
-- addOwnProperty
--   :: [EssenceVL]
--   -> (EssenceVL, PropertyVL)
--   -> PropertyVL
--   -> PropertyVL
-- addOwnProperty [] childProp prop =
--   addOwnTopLevelProperty childProp prop
-- addOwnProperty _ childProp prop =
--   -- TODO
--   error "adding prop into a bag not implemented"




-- addSharedProperty'
--   :: (EssenceVL, PropertyVL)
--   -> PropertyVL
--   -> PropertyVL
-- addSharedProperty' (ess, childProp) (PropDict root propKVs) = let
--   propKV = PropKeyVal ess (SharedProp childProp)
--   in PropDict root (propKV : propKVs)
-- addSharedProperty' _ _ = error "Invalid parent property (not a dict)"

-- addSharedProperty
--   :: [EssenceVL]
--   -> (EssenceVL, PropertyVL)
--   -> PropertyVL
--   -> PropertyVL
-- addSharedProperty [] childProp prop = addSharedProperty' childProp prop
-- addSharedProperty _ childProp prop =
--   -- TODO
--   error "adding prop into a bag not implemented"
