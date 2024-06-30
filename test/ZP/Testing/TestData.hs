{-# LANGUAGE DataKinds #-}

module ZP.Testing.TestData where

import ZP.Prelude

import ZP.Domain.Static.Model
import qualified ZP.Assets.KnowledgeBase as KB
import ZP.Assets.KnowledgeBase.Common
import ZP.Assets.KnowledgeBase.Essences

import Data.Proxy


type TestIconOwning = OwnVal (KB.IconVal "+")
type TestPropKeyVal = PropKeyVal EIcon TestIconOwning

type TestProp = DerivedProp EIntrinsics AnyProp
  '[ TestPropKeyVal
   ]
  '[]

-- Test world

-- | General root property for everything that is open.
type Open  = TagProp (TagGroup EOpen)
type Close = TagProp (TagGroup EClose)

type StateOpen  = TagProp (TagGroupRoot EStateOpen  Open)
type StateClose = TagProp (TagGroupRoot EStateClose Close)

type PushableScript = 'Script @'TypeLevel "'pushable' ability script" '[]

type CloseStateRef = '[ EStates, EStateClose ]
type OpenStateRef  = '[ EStates, EStateOpen  ]

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


type World1 = WorldData @TypeLevel
  '[ "#############"
   , "#...........#"
   , "###?####.####"
   , "#...........#"
   , "#############"
   ]

type Wall = DerivedProp EWall AnyProp
  '[ PropKeyVal EIcon (OwnVal (IconVal "#"))
   , PropKeyVal EPos  (OwnVal DerivablePosTagVal)
   ]
  '[]

type EmptySpace = DerivedProp EEmptySpace AnyProp
  '[ PropKeyVal EIcon (OwnVal (IconVal "."))
   , PropKeyVal EPos  (OwnVal DerivablePosTagVal)
   ]
  '[]

type Zeplrog world = GameEnvironment
  world
  PathToIconRel
  PathToPosRel

  -- Static props for the instantiation from the world data
  '[ EmptySpace
   , SpecificDoor
   , Wall
   ]

  -- Objects (static props placed into the world separately)
  '[ Obj 2 8 GenericDoor
   , Obj 3 9 Wall
   ]
