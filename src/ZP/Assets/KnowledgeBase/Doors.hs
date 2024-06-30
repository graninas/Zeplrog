{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.Doors where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Assets.KnowledgeBase.Essences
import ZP.Assets.KnowledgeBase.Common

import GHC.TypeLits


-- | General root property for everything that is open.
type Open  = TagProp (TagGroup EOpen)
type Close = TagProp (TagGroup EClose)

type StateOpen  = TagProp (TagGroupRoot EStateOpen  Open)
type StateClose = TagProp (TagGroupRoot EStateClose Close)

type PushableScript = 'Script @'TypeLevel "'pushable' ability script" '[]
  -- '[]
  -- '[ SimpleQuery
  --       '[ FollowReferences ]
  --       '[ QEssence EState, QGetEssence ]
  --        (BoolVar "is open")
  --  ]
  -- '[ ConditionalAction
  --     (ConditionDef "is open" QEq (BoolValue True))
  --     (ReplaceProp '[ EState ] '[ EStates, EStateClose ])
  --  , ConditionalAction
  --     (ConditionDef "is open" QEq (BoolValue False))
  --     (ReplaceProp '[ EState ] '[ EStates, EStateOpen ])
  --  ]

type CloseStateRef = RelPath '[ EStates, EStateClose ]
type OpenStateRef  = RelPath '[ EStates, EStateOpen  ]

-- | Abstract door.
type AbstractDoor = AbstractProp (Group EAbstractDoor)
  '[ PropKeyVal EIcon (OwnVal (IconVal "+"))   -- TODO: open and close door with own icons
   , PropKeyVal EHP   (OwnVal (HPTagVal 50))
   , PropKeyVal EPos  (OwnVal DerivablePosTagVal)

    -- | Possible states
   , PropKeyBag EStates
      '[ TagPropRef StateOpen
       , TagPropRef StateClose
       ]

    -- | Current state. Points to a close/open state
   , PropKeyVal EState (OwnVal (PathValue CloseStateRef))

  --   -- | Abilities to react to effects
  --  , PropKeyBag EAbilities
  --     '[ SharedProp (PropScript (Group EPushable)
  --                   PushableScript)
  --      ]
   ]
   '[ PropScript EPushable PushableScript
   ]


-- | Specific door with a specific icon.
type SpecificDoor = DerivedProp ESpecificDoor AbstractDoor
  '[ PropKeyVal EIcon (OwnVal (IconVal "?"))   -- TODO: open and close door with own icons
   , PropKeyVal EHP   (OwnVal (HPTagVal 100))
   , PropKeyVal EPos  (OwnVal (PosTagVal 2 3))
   ]
  '[]

-- | Template for all doors having no predefined location.
--   Derived from AbstractDoor.
type GenericDoor = DerivedProp EGenericDoor AbstractDoor
  '[ PropKeyVal EHP (OwnVal (HPTagVal 70))
   ]
  '[]


-- Door
--   EPos <shared> -------------> (PosConst 3 5)
--   EHP  <own> (HPVal 100)
--              ==> Immortal
--
-- #2 dog
--   - #3 hp: 23
--   - #4 pos: (2,5)
-- (dog has intrinsic ability to push. It pushes door #1)
-- (dog has intrinsic ability to discover.
--  It discovers (2,6) is pushable.
--  It has ability to push. It pushes secret door #10)
--
--
--
-- #1 Door
--   - #6 hp: 100
--   - #7 pos: (3,5) -> #20 passable: true
--                          => # `passable`                -- static prop ref
--   - #8 state: ref
--        => # `state: close` -> script for `passable`     -- static prop ref
--        => # `state: open`  -> script for `passable`     -- static prop ref
--   - #99 pushable
--         => # `pushable` -> script for `state`
--
-- #10 Secret door
--   - #65 hp: 100
--   - #75 pos: (2,6) -> #21 passable: false
--                           => # `passable`
--   - #85 state: ref
--         => # `state: open`  -> script for `passable`     -- static prop ref
--         => # `state: close` -> script for `passable`     -- static prop ref
--   - #95 pushable
--         => # `pushable` -> script1 for `state`
--
--
-- #100 Lever
--   - #66 hp: 20
--   - #76 pos: (22,5) -> #22 passable: true
--                            => # `passable`
--   - #86 state: ref
--         => # `state: up`
--         => # `state: down`
--   - #100 pushable
--         => # `pushable` -> script2 for `state`
--
--
--
--
--
--
-- Effects
--   `push`      -> `physical impact`
--   `shockwave` -> `physical impact`
--   `kick`      -> `physical impact`
--
--   `physical impact`
--     [ `pushable` -> activate script
--     ]
--
-- Dilemmas:
--  * Open/close can be separate properties
--  * Open/close value can be an intrinsic
--      OR "current property"
--  * Open/close action can be outside of the iternal doors
--      OR it can be a part of the external doors
--  * API can be an external graph link connecting doors
--      OR it can be internal API connecting separate internal open/close properties



