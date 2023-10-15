# Important Type Tricks


**Type-level static, value-level static and value-level dynamic models**


**HKD and field type selection**

Specific mechanism (a kind of the HKD pattern)
to choose between Symbol and String, Nat and Int
when using the model either as type-level or as value-level one.
Very boilerplaity. Has a high accidental complexity.
Alternative: a completely separate yet almost identical
of set of types for the static value-level model
with Strings instead Symbols and Ints instead Nats.

```haskell
data Level = TypeLevel | ValueLevel

type family StringType (lvl :: Level) where
  StringType 'TypeLevel  = Symbol
  StringType 'ValueLevel = String

type family IntegerType (lvl :: Level) where
  IntegerType 'TypeLevel  = Nat
  IntegerType 'ValueLevel = Int

data Essence (lvl :: Level) where
  Ess :: StringType lvl -> Essence lvl
```

**Macroses over type-level model**

**Type-level variable bindings (equations)**

```haskell
mkE :: forall ess symb
    . KnownSymbol symb
    => (ess ~ 'SMod.Ess @SMod.TypeLevel symb)
    => Essence
mkE = symbolVal $ Proxy @symb
```

**Type lists interpretation with helper empty ADTs**

```haskell
data Props props

instance
  SMat p (Props '[]) [()] where
  sMat p _ = pure []

instance
  ( SMat p prop (EssenceVL, PropertyVL)
  , SMat p (Props props) [(EssenceVL, PropertyVL)]
  ) =>
  SMat p (Props (prop ': props)) [(EssenceVL, PropertyVL)] where
  sMat p _ = do
    prop  <- sMat p $ Proxy @prop
    props <- sMat p $ Proxy @(Props props)
    pure $ prop : props

```

**Newtype approach on the type level**

Newtype safety works on the type level.

```haskell
data IconEssencePath (lvl :: Level) where
  IconPath :: EssencePath lvl -> IconEssencePath lvl

data PosEssencePath (lvl :: Level) where
  PosPath :: EssencePath lvl -> PosEssencePath lvl
```

**Type-level and value-level constructors**

Type-level and value-level constructors to support a good UX
and not invent additional data types:

```haskell
data PropertyGroup (lvl :: Level) where
  -- | Property groups for static type-level representation.
  Group     :: EssenceTL -> PropertyGroupTL
  GroupRoot :: EssenceTL -> PropertyTL -> PropertyGroupTL

  -- | Property groups and id for static value-level representation.
  GroupId      :: EssenceVL -> StaticPropertyId -> PropertyGroupVL
  GroupRootId  :: EssenceVL -> StaticPropertyId -> PropertyVL -> PropertyGroupVL
```

Alternative would be making a type family for the id type:

```haskell
data PropId where
  PropId :: EssenceVL -> StaticPropertyId -> PropId

type family PropertyIdType (lvl :: Level) where
  PropertyIdType 'TypeLevel  = EssenceTL
  PropertyIdType 'ValueLevel = PropId

data PropertyGroup (lvl :: Level) where
  -- | Property groups for static type-level representation.
  Group     :: PropertyIdType lvl -> PropertyGroupTL
  GroupRoot :: PropertyIdType lvl -> PropertyTL -> PropertyGroupTL
```

This has a bad impact on the UX of user-defined data types
and requires more @TypeLevel specifications than usual:

```haskell
type EOpen = Ess @TypeLevel "open"
type Open  = StaticProp @TypeLevel (Group EOpen) -- extra @TypeLevel here
```
