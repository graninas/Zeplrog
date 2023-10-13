{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZP.Domain.Static.Model.Property where

import ZP.Prelude
import GHC.TypeLits
import qualified Text.Show as T

import ZP.Domain.Static.Model.Common
import ZP.Domain.Static.Model.Script

------ Property -----

-- | Used to make static property hierarchies.
-- Essence arg: own essence
-- Property arg: parent property
data StaticPropertyRoot (lvl :: Level) where
  EssStaticRoot   :: Essence lvl -> StaticPropertyRoot lvl
  PropStaticRoot  :: Essence lvl -> Property lvl -> StaticPropertyRoot lvl

-- Property owning replaces materialization from the prev version
data PropertyOwning (lvl :: Level) where
  -- | Property will be materialized for each parent prop.
  OwnProp    :: Property lvl -> PropertyOwning lvl
  -- | Property will be materialized only once and shared between parents.
  SharedProp :: Property lvl -> PropertyOwning lvl

data PropertyKeyValue (lvl :: Level) where
  -- | Implicit dictionary of properties.
  -- When materialized, becomes a dict with keys taken from properties
  PropKeyBag :: Essence lvl -> [PropertyOwning lvl] -> PropertyKeyValue lvl
  -- | Separate property
  PropKeyVal :: Essence lvl -> PropertyOwning lvl -> PropertyKeyValue lvl

-- | Static property that must be stat and dyn materialized.
data Property (lvl :: Level) where
  -- | Static prop. Referenced prop can't be dyn materialized.
  StaticProp
    :: StaticPropertyRoot lvl
    -> Property lvl
  -- | Static prop reference. The referenced prop can't be dyn materialized.
  StaticPropRef
    :: Property lvl
    -> Property lvl
  -- | Lear prop. Value will be dyn materialized as mutable var (TVar).
  PropVal
    :: StaticPropertyRoot lvl
    -> ValDef lvl
    -> Property lvl
  -- | Compound property.
  -- Each prop in the bag is a mutable reference.
  -- Each prop can be replaced by some other prop in the dyn model.
  PropDict
    :: StaticPropertyRoot lvl
    -> [PropertyKeyValue lvl]
    -> Property lvl
  -- | Property script.
  PropScript
    :: StaticPropertyRoot lvl
    -> Script lvl
    -> Property lvl


------ Short identifiers ----------

type PropertyTL = Property 'TypeLevel
type PropertyVL = Property 'ValueLevel

type StaticPropertyRootTL = StaticPropertyRoot 'TypeLevel
type StaticPropertyRootVL = StaticPropertyRoot 'ValueLevel

type PropertyKeyValueTL = PropertyKeyValue 'TypeLevel
type PropertyKeyValueVL = PropertyKeyValue 'ValueLevel

type PropertyOwningTL = PropertyOwning 'TypeLevel
type PropertyOwningVL = PropertyOwning 'ValueLevel
