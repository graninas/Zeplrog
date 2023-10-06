{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module ZP.Typed.KnowledgeBase.Doors where

import ZP.Typed.Model.Property
import ZP.Typed.KnowledgeBase.Essences
import ZP.Typed.KnowledgeBase.Common

import GHC.TypeLits

-- TODO: condition value
-- type OpeningCondition = Prop (PropRootEss EToOpen)
-- type ClosingCondition = Prop (PropRootEss EToClose)


-- type CloseState = PropDict (PropRootEss EClose)
--   '[ PropBag EActivations
--      '[ SharedProp OpeningCondition
--       ]
--    ]
-- type Door = PropDict (PropRootEss EDoor)
--   '[ PropBag EStates
--      '[ SharedProp OpenState
--       , SharedProp CloseState
--       ]
--    , PropBag EIntrinsics
--      '[ OwnProp HP         -- can be set statically
--       , OwnProp Pos        -- depends on the map
--       ]
--    ]
--
-- --
-- type OpeningCondition2 = Query
--   '[ QueryBag EIntrinsics
--    , QueryPropVal EStateVal (MatchVal (EssenceVal EOpenState))
--    ]

-- type OpenCmd = PubMethod "open command"
--               (Cmd EOpenCmd)
--               OpeningCondition2
--
-- --------
--

-- | General root property for everything that is open.
type Open  = StaticProp (EssStaticRoot EOpen)
type Close = StaticProp (EssStaticRoot EClose)

type StateOpen  = StaticProp (PropStaticRoot EStateOpen Open)
type StateClose = StaticProp (PropStaticRoot EStateClose Close)

type OpenStateRef = PropRef
  '[ EStates
   , EStateOpen
   ]

type CloseStateRef = PropRef
  '[ EStates
   , EStateClose
   ]

type PushableScript = Script EPushable
  '[ PreQuery QueryOpen           -- assuming query returns True or False
      (BoolValue "is open" True)  -- TODO: query returns any values.
      (BoolValue "is open" False)
   ]
  '[ Action
      (Condition "is open" True)
      (ReplacePropRef '[ EState ] CloseStateRef)
   , Action
      (Condition "is open" False)
      (ReplacePropRef '[ EState ] OpenStateRef)
   ]

type Door = PropDict (EssRoot EDoor)
  '[ PropKeyVal EHP (OwnProp (HPVal 100))
   , PropKeyVal EPos (SharedProp (PosConst 3 5))        -- TODO: identified from the map

    -- | Current state
   , PropKeyVal EState (OwnProp OpenStateRef)

    -- | Possible states
   , PropKeyBag EStates
      '[ OwnProp (StaticPropRef StateOpen)
       , OwnProp (StaticPropRef StateClose)
       ]

   , PropKeyBag EAbilities
      '[ SharedProp (PropScript PushableScript)
       ]
   ]
--
--
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
--  * Open/close can be a separate properties
--  * Open/close value can be an intrinsic
--      OR a "current property"
--  * Open/close action can be outside of the iternal doors
--      OR it can be a part of the external doors
--  * API can be an external graph link connecting doors
--      OR it can be internal API connecting separate internal open/close properties



-- doorStaticProperty :: CommonStaticProperties -> KBBuilder StaticProperty
-- doorStaticProperty csp@(CommonStaticProperties{posSProp, hpSProp}) = do
--   openingCondSProp <- openingConditionStaticProperty csp
--   closingCondSProp <- closingConditionStaticProperty csp

--   openStateSProp   <- openStateStaticProperty   csp closingCondSProp
--   closedStateSProp <- closedStateStaticProperty csp openingCondSProp

--   -- Making a loop of states
--   lift $ writeTVar (staticPropertyValueVar openingCondSProp)
--        $ MaterializableStateValue "to open state"
--        $ SharedMaterialization openStateSProp
--   lift $ writeTVar (staticPropertyValueVar closingCondSProp)
--        $ MaterializableStateValue "to closed state"
--        $ SharedMaterialization closedStateSProp

--   currentStateVar <- lift $ newTVar $ MaterializableStateValue "cur state" (SharedMaterialization closedStateSProp)

--   let props = Map.fromList
--         [ (statesPropType,    [ (SharedMaterialization openStateSProp)
--                               , (SharedMaterialization closedStateSProp) ])
--         , (inventoryPropType, [ (DirectMaterialization posSProp)
--                               , (DirectMaterialization hpSProp) ])
--         ]

--   mkStaticProperty doorEssence "" props currentStateVar StaticDiscoverRoot ActiveValueDiscoverable



-- ratStaticProperty :: CommonStaticProperties -> KBBuilder StaticProperty
-- ratStaticProperty CommonStaticProperties{..} = do
--   valVar <- lift $ newTVar NoStaticValue
--   let props = Map.fromList
--         [ (inventoryPropType, [ DirectMaterialization posSProp
--                               , DirectMaterialization hpSProp ])
--         , (actionsPropType,   [ DirectMaterialization noActionSProp ])              -- this rat doesn't do anything...
--         ]
--   mkStaticProperty ratEssence "" props valVar StaticDiscoverRoot ActiveValueNonDiscoverable



-- guardStaticProperty
--   :: StaticProperty
--   -> CommonStaticProperties
--   -> KBBuilder StaticProperty
-- guardStaticProperty ratSProp commonSProps@(CommonStaticProperties{..}) = do
--   valVar <- lift $ newTVar NoStaticValue
--   -- TODO: add a new type of a dynamic discoverability: discoverability on usage
--   killRatGoalSProp <- killGoalStaticProperty ratSProp
--   let props = Map.fromList
--         [ (inventoryPropType, [ DirectMaterialization posSProp
--                               , DirectMaterialization hpSProp
--                               , DirectMaterialization fireWandSProp
--                               , DirectMaterialization iceWandSProp ])
--         , (actionsPropType, map DirectMaterialization $ commonActionsSProps commonSProps)
--         , (goalsPropType,     [ DirectMaterialization killRatGoalSProp ])
--         ]
--   mkStaticProperty guardEssence "" props valVar StaticDiscoverRoot ActiveValueNonDiscoverable



