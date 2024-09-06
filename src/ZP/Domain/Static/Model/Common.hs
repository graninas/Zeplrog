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
import GHC.Types as GHC

import qualified Text.Show as T
import qualified Data.Kind as DK

------ Common and General -----------------

-- | Sudo ID of a property

data Essence (lvl :: Level) where
  Ess :: StringType lvl -> Essence lvl

data EssencePath (lvl :: Level) where
  RelPath :: [Essence lvl] -> EssencePath lvl
  AbsPath :: [Essence lvl] -> EssencePath lvl

-- | Real Id of a property (for value-level usage only)

newtype StaticPropertyId = StaticPropertyId Int
  deriving (Show, Eq, Ord)

-- | Type tag

data CustomTag where
  RegularTag  :: Symbol -> CustomTag
  CompoundTag :: Symbol -> CustomTag -> CustomTag -> CustomTag

type TagName = String

type DEssence = String
data DEssencePath
  = DRelPath [DEssence]
  | DAbsPath [DEssence]
  deriving (Show, Eq, Ord)

-- | Dynamic value needed to avoid much complexities
--   of dependent typing
data DValue
  = PairValue TagName DValue DValue
  | IntValue TagName Int
  | BoolValue TagName Bool
  | StringValue TagName String
  | TagValue TagName TagPropertyVL DValue
  | PathValue TagName DEssencePath
  | StaticPropertyRefValue TagName StaticPropertyId
  | AnyValue TagName (Maybe String) GHC.Any
  | DPlaceholder

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
type family TagToType (lvl :: Level) (tag :: CustomTag) :: *

type instance TagToType lvl IntTag     = IntegerType lvl
type instance TagToType lvl StringTag  = StringType lvl
type instance TagToType lvl BoolTag    = Bool
type instance TagToType lvl EssenceTag = Essence lvl
type instance TagToType lvl PathTag    = EssencePath lvl
type instance TagToType 'TypeLevel  (TPHTag vt) = TagPropertyValueHolder 'TypeLevel vt
type instance TagToType 'ValueLevel (TPHTag vt) = TagPropertyValueHolder 'ValueLevel vt
type instance TagToType lvl PairIntIntTag = CustomPair (IntegerType lvl) (IntegerType lvl)


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
type TPHTag (innerT :: CustomTag) = 'CompoundTag "TPH" innerT ('RegularTag "")

data CustomPair a b = Pair a b
data TagPropertyValueHolder lvl (tag :: CustomTag)
  = TPH
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

type PathValue (path :: EssencePathTL)
  = GenericValue @'TypeLevel @PathTag path 'DPlaceholder

type IntPairValue (i1 :: Nat) (i2 :: Nat)
  = GenericValue @'TypeLevel @PairIntIntTag ('Pair i1 i2) 'DPlaceholder

type TagPropertyValue
  (tagProp :: TagProperty 'TypeLevel)
  (genVal :: GenericValDef 'TypeLevel vt)
  = GenericValue @TypeLevel @(TPHTag vt)
      (TPH tagProp genVal) 'DPlaceholder

-- Predefined constants

type IntConst (i :: Nat)
  = GenericConst (IntValue i)

type BoolConst (b :: Bool)
  = GenericConst (BoolValue b)

type StringConst (s :: Symbol)
  = GenericConst (StringValue s)

type EssenceConst (ess :: EssenceTL)
  = GenericConst (EssenceValue ess)

type PathConst (path :: EssencePathTL)
  = GenericConst (PathValue path)

type IntPairConst (i1 :: Nat) (i2 :: Nat)
  = GenericConst (IntPairValue i1 i2)

type TagPropertyValueConst
  (tagProp :: TagProperty 'TypeLevel)
  (genVal :: GenericValDef 'TypeLevel vt)
  = GenericConst (TagPropertyValue tagProp genVal)

------ Short identifiers ----------

type EssenceTL = Essence 'TypeLevel
type EssenceVL = Essence 'ValueLevel

type EssencePathTL = EssencePath 'TypeLevel
type EssencePathVL = EssencePath 'ValueLevel

type TagPropertyGroupTL = TagPropertyGroup 'TypeLevel
type TagPropertyGroupVL = TagPropertyGroup 'ValueLevel

type TagPropertyTL = TagProperty 'TypeLevel
type TagPropertyVL = TagProperty 'ValueLevel

type GenericValDefTL = GenericValDef 'TypeLevel
type GenericValDefVL = GenericValDef 'ValueLevel

type GenericConstDefTL = GenericConstDef 'TypeLevel
type GenericConstDefVL = GenericConstDef 'ValueLevel

type TagPropertyValueHolderTL = TagPropertyValueHolder 'TypeLevel
type TagPropertyValueHolderVL = TagPropertyValueHolder 'ValueLevel

-------- Instances ------------------

instance Eq EssenceVL where
  (==) (Ess a) (Ess b) = a == b

instance Ord EssenceVL where
  compare (Ess a) (Ess b) = compare a b

instance T.Show EssenceVL where
  show (Ess a) = T.show a

instance Eq EssencePathVL where
  (==) (RelPath a) (RelPath b) = a == b
  (==) (AbsPath a) (AbsPath b) = a == b
  (==) _ _ = False

instance Ord EssencePathVL where
  compare (RelPath a) (RelPath b) = compare a b
  compare (AbsPath a) (AbsPath b) = compare a b
  compare (RelPath _) _ = LT
  compare _ _ = GT

instance T.Show EssencePathVL where
  show (RelPath a) = "|Rel|" <> show a <> "|"
  show (AbsPath a) = "|Abs|" <> show a <> "|"

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


class Tag (tag :: CustomTag) where
  tagToString :: Proxy tag -> String

instance
  ( KnownSymbol s
  ) => Tag ('RegularTag s) where
  tagToString _ = symbolVal $ Proxy @s

instance
  ( KnownSymbol s
  ) => Tag ('CompoundTag s v1 v2) where
  tagToString _ = symbolVal $ Proxy @s

tagName :: DValue -> TagName
tagName (PairValue tn _ _) = tn
tagName (IntValue tn _) = tn
tagName (BoolValue tn _) = tn
tagName (StringValue tn _) = tn
tagName (TagValue tn _ _) = tn
tagName (PathValue tn _) = tn
tagName (StaticPropertyRefValue tn _) = tn
tagName (AnyValue tn _ _) = tn
tagName DPlaceholder = "DPlaceholder"

mkIntValue :: Int -> DValue
mkIntValue x = IntValue (tagToString (Proxy @IntTag)) x

mkIntPairValue :: Int -> Int -> DValue
mkIntPairValue x y =
  PairValue
    (tagToString (Proxy @PairIntIntTag))
    (mkIntValue x)
    (mkIntValue y)

mkPathValue :: DEssencePath -> DValue
mkPathValue path =
  PathValue (tagToString (Proxy @PathTag)) path

pathLength :: EssencePathVL -> Int
pathLength (RelPath path) = length path
pathLength (AbsPath path) = length path


showDValue :: DValue -> String
showDValue (PairValue _ p1 p2) = "(" <> showDValue p1 <> ", " <> showDValue p2 <> ")"
showDValue (IntValue _ p) = show p
showDValue (BoolValue _ p) = show p
showDValue (StringValue _ p) = p
showDValue (TagValue _ _ dv) = "TagValue " <> showDValue dv
showDValue (PathValue _ p) = show p
showDValue (StaticPropertyRefValue _ sId) = "StaticPropertyRefValue " <> show sId
showDValue (AnyValue _ Nothing _) = "AnyValue: not showable"
showDValue (AnyValue _ (Just p) _) = "AnyValue: " <> p
showDValue DPlaceholder = "DPlaceholder"


-- Equality of dyn values. Should be used with care.
-- Maybe, use it only for tests
eqDValue :: DValue -> DValue -> Bool
eqDValue (PairValue t1 p1 p2)
         (PairValue t2 p3 p4)
          = t1 == t2 && eqDValue p1 p3 && eqDValue p2 p4
eqDValue (IntValue t1 p1)
         (IntValue t2 p2)                = t1 == t2 && p1 == p2
eqDValue (BoolValue t1 p1)
         (BoolValue t2 p2)               = t1 == t2 && p1 == p2
eqDValue (StringValue t1 p1)
         (StringValue t2 p2)             = t1 == t2 && p1 == p2
eqDValue (TagValue t1 _ p1)
         (TagValue t2 _ p2)              = t1 == t2 && eqDValue p1 p2
eqDValue (PathValue t1 p1)
         (PathValue t2 p2)               = t1 == t2 && p1 == p2
eqDValue (StaticPropertyRefValue t1 sId1)
         (StaticPropertyRefValue t2 sId2)
        = t1 == t2 && sId1 == sId2
eqDValue (AnyValue t1 s1 _)
         (AnyValue t2 s2 _)
          = t1 == t2 && s1 == s2      -- N.B., not a good equality
                                      -- because different GHC.Any
                                      -- can occur equal
eqDValue DPlaceholder DPlaceholder = True
eqDValue _ _ = False
