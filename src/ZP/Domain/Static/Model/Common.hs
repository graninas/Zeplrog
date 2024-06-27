{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module ZP.Domain.Static.Model.Common where

import ZP.Prelude
import GHC.TypeLits

import qualified Text.Show as T
import qualified Data.Kind as DK

------ Common and General -----------------

-- | Sudo ID of a property

data Essence (lvl :: Level) where
  Ess :: StringType lvl -> Essence lvl

type EssencePath (lvl :: Level) = [Essence lvl]

-- | Real Id of a property (for value-level usage only)

newtype StaticPropertyId = StaticPropertyId Int
  deriving (Show, Eq, Ord)

-- | Type tag

data CustomTag where
  RegularTag  :: Symbol -> CustomTag
  CompoundTag :: Symbol -> CustomTag -> CustomTag -> CustomTag

-- | Dynamic value needed to avoid much complexities
--   of dependent typing
data DValue
  = PairValue DValue DValue
  | IntValue Int
  | BoolValue Bool
  | StringValue String
  | TagValue TagPropertyVL DValue
  | PathValue [String]
  | StaticPropertyRefValue StaticPropertyId
  | DPlaceholder
  deriving (Show, Eq, Ord)

-- | Tag property is always static.
--   Used to tag and group notions.
--   Can be hierarchical.
data TagPropertyGroup (lvl :: Level) where
  -- | Tag property groups for static type-level representation.
  TagGroup     :: Essence lvl -> TagPropertyGroup lvl
  TagGroupRoot :: Essence lvl -> TagProperty lvl -> TagPropertyGroup lvl

-- | Tag property: immutable, reference-only,
--   one instance, only for grouping.
data TagProperty (lvl :: Level) where
  -- | Tag prop for static type-level and
  --   dynamic value-level representation.
  TagProp
    :: TagPropertyGroup lvl
    -> TagProperty lvl

-- | Type family that will match a type tag
--   to a specific type depending on the level
type family TagToType (lvl :: Level) (tag :: CustomTag) where
  TagToType lvl IntTag     = IntegerType lvl
  TagToType lvl StringTag  = StringType lvl
  TagToType lvl BoolTag    = Bool
  TagToType lvl EssenceTag = Essence lvl
  TagToType lvl PathTag    = [Essence lvl]
  TagToType 'TypeLevel  (TVHTag vt) = TagValueHolder 'TypeLevel vt
  TagToType 'ValueLevel (TVHTag vt) = TagValueHolder 'ValueLevel vt
  TagToType lvl PairIntIntTag = CustomPair (IntegerType lvl) (IntegerType lvl)


-- | Generic value definition with a default value
data GenericValDef (lvl :: Level) (tag :: CustomTag) where
  GenericValue
    :: TagToType lvl tag       -- ^ Type family that adjusts
                               -- the actual field type depending on the level and type tag
    -> DValue
    -> GenericValDef lvl tag

-- | Constant definition
data GenericConstDef (lvl :: Level) (tag :: CustomTag) where
  GenericConst
    :: GenericValDef lvl tag   -- ^ Constant value
    -> GenericConstDef lvl tag

------------- Predefined values and types

-- Predefined types and tags
type IntTag         = 'RegularTag "int"
type BoolTag        = 'RegularTag "bool"
type StringTag      = 'RegularTag "string"
type PathTag        = 'RegularTag "path"
type TagTag         = 'RegularTag "tag:tag"
type EssenceTag     = 'RegularTag "essence"
type PairIntIntTag  = 'CompoundTag "int pair" IntTag IntTag
type TVHTag (innerT :: CustomTag) = 'CompoundTag "TVH" innerT ('RegularTag "")

data CustomPair a b = Pair a b
data TagValueHolder lvl (tag :: CustomTag)
  = TVH
    (TagProperty lvl)
    (GenericValDef lvl tag)

-- Predefined values

type IntValue (i :: Nat)
  = GenericValue @'TypeLevel @IntTag i 'DPlaceholder

type BoolValue (b :: Bool)
  = GenericValue @'TypeLevel @BoolTag b 'DPlaceholder

type StringValue (s :: Symbol)
  = GenericValue @'TypeLevel @StringTag s 'DPlaceholder

type EssenceValue (ess :: EssenceTL)
  = GenericValue @'TypeLevel @EssenceTag ess 'DPlaceholder

type PathValue (ss :: [EssenceTL])
  = GenericValue @'TypeLevel @PathTag ss 'DPlaceholder

type IntPairValue (i1 :: Nat) (i2 :: Nat)
  = GenericValue @'TypeLevel @PairIntIntTag ('Pair i1 i2) 'DPlaceholder

type TagValue
  (tagProp :: TagProperty 'TypeLevel)
  (genVal :: GenericValDef 'TypeLevel vt)
  = GenericValue @TypeLevel @(TVHTag vt)
      (TVH tagProp genVal) 'DPlaceholder



-- Predefined constants

type IntConst (i :: Nat)
  = GenericConst (IntValue i)

type BoolConst (b :: Bool)
  = GenericConst (BoolValue b)

type StringConst (s :: Symbol)
  = GenericConst (StringValue s)

type EssenceConst (ess :: EssenceTL)
  = GenericConst (EssenceValue ess)

type PathConst (s :: [EssenceTL])
  = GenericConst (PathValue s)

-- TODO: rest of consts


------ Short identifiers ----------

type EssenceTL = Essence 'TypeLevel
type EssenceVL = Essence 'ValueLevel

type TagPropertyGroupTL = TagPropertyGroup 'TypeLevel
type TagPropertyGroupVL = TagPropertyGroup 'ValueLevel

type TagPropertyTL = TagProperty 'TypeLevel
type TagPropertyVL = TagProperty 'ValueLevel

type GenericValDefTL = GenericValDef 'TypeLevel
type GenericValDefVL = GenericValDef 'ValueLevel

type GenericConstDefTL = GenericConstDef 'TypeLevel
type GenericConstDefVL = GenericConstDef 'ValueLevel

type EssencePathTL = EssencePath 'TypeLevel
type EssencePathVL = EssencePath 'ValueLevel

type TagValueHolderTL = TagValueHolder 'TypeLevel
type TagValueHolderVL = TagValueHolder 'ValueLevel

-------- Instances ------------------

instance Eq EssenceVL where
  (==) (Ess a) (Ess b) = a == b

instance Ord EssenceVL where
  compare (Ess a) (Ess b) = compare a b

instance T.Show EssenceVL where
  show (Ess a) = T.show a

instance Eq TagPropertyGroupVL where
  (==) (TagGroup a) (TagGroup b) = a == b
  (==) (TagGroupRoot a l) (TagGroupRoot b r) = (a == b) && (l == r)
  (==) _ _ = False

instance Ord TagPropertyGroupVL where
  compare (TagGroup a) (TagGroup b) = compare a b
  compare (TagGroupRoot a l) (TagGroupRoot b r)
    = compare (compare a b) (compare l r)
  compare (TagGroup _) (TagGroupRoot _ _) = GT
  compare _ _ = LT

instance T.Show TagPropertyGroupVL where
  show (TagGroup a) = "TagGroup " <> T.show a
  show (TagGroupRoot a l)
    = "TagGroupRoot " <> T.show a <> " " <> T.show l

instance Eq TagPropertyVL where
  (==) (TagProp a) (TagProp b) = a == b

instance Ord TagPropertyVL where
  compare (TagProp a) (TagProp b) = compare a b

instance T.Show TagPropertyVL where
  show (TagProp a) = "TagProp " <> T.show a

