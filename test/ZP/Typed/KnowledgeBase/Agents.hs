{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module ZP.Typed.KnowledgeBase.Agents where

import ZP.Typed.Model.Property
import ZP.Typed.KnowledgeBase.Essences
import ZP.Typed.KnowledgeBase.Common

import GHC.TypeLits


-- Why this is not a compound object?
type FireWand = StaticProp (EssStaticRoot EWand)
type IceWand  = StaticProp (EssStaticRoot EWand)
type KillGoal = StaticProp (EssStaticRoot EGoal)


type RatActor = PropDict (EssRoot ERat)
  '[ PropKeyBag EIntrinsics '[]
   ]

type GuardActor = PropDict (EssRoot EGuard)
  '[ PropKeyBag EIntrinsics '[]
   ]


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



