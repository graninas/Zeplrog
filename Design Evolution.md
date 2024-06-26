# Design Evolution

### Stages of Development

The code has gone through several stages of development.

**Stage 0: Concept Development**

The concept of ontology for in-game object properties was devised and developed. A regular query language for the ontology graph was invented.

**Stage 1: Early Mixed-Level Design**

A simple graphical application was created in which the main character went through several activity stages, observed the surroundings, discovered a door, and built a path to the door. The application included basic pathfinding and trajectory animation.

Simultaneously, the core model of static and dynamic properties was developed and tested. This was implemented using algebraic data types at a mixed level (values + types). A schema for materializing the static model into the dynamic one was created. Test data was developed, and tests were created.

**Stage 2: Zeplrog as a Showcase Project for PTLD Book**

The previous model and application were removed.

The property structure itself was reworked. A basic language for property activation scripts was created.

A new model of static properties was developed at the type level, following the methodology outlined in the Pragmatic Type-Level Design book.

Test examples at the type level were developed.

A model for dynamic properties was created.

Partial work on materializing the static model into the dynamic one was carried out.

Some materialization tests were created.

**Stage 2, Iteration 1**

The model for static properties is implemented using types. Properties described using this model will be treated as types.

```haskell
type Door = PropDict (EssRoot EDoor)
  '[ PropKeyVal EHP (OwnProp (HPVal 100))
   , PropKeyVal EPos (SharedProp (PosConst 3 5))

    -- | Possible states
   , PropKeyBag EStates
      '[ OwnProp (StaticPropRef StateOpen)
       , OwnProp (StaticPropRef StateClose)
       ]

    -- | Current state
   , PropKeyVal EState (OwnProp OpenStateRef)

    -- | Abilities to react to effects
   , PropKeyBag EAbilities
      '[ SharedProp (PropScript PushableScript)
       ]
   ]

```

Materialization is based on type classes that convert the static typed model into the dynamic one.

```haskell
data Env = Env
  { sharedProps :: TVar (Map.Map DMod.Essence DMod.DynamicProperty)
  }

type Materializer a = ReaderT Env IO a

type Shared = Bool

-- | Materialization type class.
class Mat a b | a -> b where
  mat :: Shared -> Proxy a -> Materializer b
```

Good side of the typed model that when a new typed data comes,
the compiler will tell if all the Mat instances are available for it.
Enables a gradual development of the model.

A Proof of Concept (PoC) mechanism for querying static model types to obtain static property values was created.

However, the mechanism proved to be overly complex. Integrating static properties (types) into dynamic types is only possible through existentials with a limited set of actions.

```haskell
class Browse tag a b | tag a -> b where
  browse :: tag -> Proxy a -> b

data StaticPropertyRef where
  StaticPropRef
    -- N.B. This is a very strange way to browse static properties.
    -- It will require A LOT boilerplate.
    -- It's a proof that static types can be accessed from
    -- the runtime.
    :: Browse GetEssence p Essence
    => Proxy (p :: SMod.Property)
    -> StaticPropertyRef
```

This solution is not easily extensible and quite verbose. It highlighted the need to reconsider the property design. A model for static properties needs to be created at a mixed level (values + types), and the compilation of property types into this model, as well as into the dynamic property model, should be done.

Thus, three models are required:

- Static at the type level
- Static at the mixed level
- Dynamic at the mixed level

As a result of compilation, the static model at the type level is no longer used, and instead, the mixed-level static model is used.

Creating three models is undoubtedly overengineering. However, the goal pursued - a showcase project for the book on type-based programming - justifies it.


**Stage 2, Iteration 2**

To support 3 levels of models and not produce a whole extra
set of types, it was decided to reuse the current static model
for the mixed level, too.

However, it turned out that it can't be used for value level
as long as it has Symbol and Nat that have no value level
representation and thus don't allow to create values of these types.

The following mechanism was introduced to overcome this problem:

```haskell
data Level
  = TypeLevel
  | ValueLevel    -- called value-level, not mixed level for convenience

type family StringType (lvl :: Level) where
  StringType 'TypeLevel  = Symbol
  StringType 'ValueLevel = String

data Essence (lvl :: Level) where
  Ess :: StringType lvl -> Essence lvl
```

Now, the types can decide what field types to use: for type-level
or for value-level depending on lvl.

However, this turned the whole model into a mess:

```haskell

data StaticProperty (lvl :: Level) where
  StaticProp :: StaticPropertyRoot lvl -> StaticProperty lvl

data Property (lvl :: Level) where
  StaticPropRef :: StaticProperty lvl -> Property lvl
  PropRef :: [Essence lvl] -> Property lvl
```

The samples got a slight update. It was enough to specify `@TypeLevel`
for only essences, and then, it was inferred for the subsequent
types:

```haskell
type EHP = Ess @TypeLevel "intrinsics:hp"
type HPVal hp = PropVal (EssRoot EHP) (IntValDef hp)
```

The old static to dynamic materialization was affected slightly:

```haskell
instance
  KnownSymbol symb =>
  Mat ('Ess @TypeLevel symb) DMod.Essence where
  mat _ _ = pure $ symbolVal (Proxy @symb)
```

This also caused a significant problem with the ugly
DynamicProperty/StaticPropertyRef existential mechanism
while materializing. Existential static ref was removed.
It should be replaced by references to the value-level static model.


**Stage 2, Iteration 3**

The idea remains the same but models were reworked much.
Static properties and references were simplified.

**Stage 3**

Complete rehaul of the model. Many simplifications, improvements
and changes.

New OOP model.
New GenericValue model.
New property structure model.
New tag properties and groups.
