{-# LANGUAGE DataKinds #-}

module ZP.TestData.World1 where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Domain.Dynamic.Model
import qualified ZP.Assets.KnowledgeBase as KB


type AbstractDoor = AbstractProp (Group KB.EAbstractDoor)
  '[ PropKeyVal KB.EIcon (OwnProp (KB.IconVal "+"))   -- TODO: open and close door with own icons
   , PropKeyVal KB.EHP   (OwnProp (KB.HPVal 50))
   , PropKeyVal KB.EPos  (OwnProp KB.DerivedPosVal)
   ]

type SpecificDoor = DerivedProp KB.ESpecificDoor AbstractDoor
  '[ PropKeyVal KB.EIcon (OwnProp (KB.IconVal "?"))   -- TODO: open and close door with own icons
   , PropKeyVal KB.EHP   (OwnProp (KB.HPVal 100))
   , PropKeyVal KB.EPos  (OwnProp (KB.PosVal 0 1))
   ]

type GenericDoor = DerivedProp KB.EDoor AbstractDoor
  '[ PropKeyVal KB.EHP (OwnProp (KB.HPVal 70))
   ]

type Wall = PropDict (Group KB.EWall)
  '[ PropKeyVal KB.EIcon (OwnProp (KB.IconVal "#"))
   , PropKeyVal KB.EPos  (OwnProp KB.DerivedPosVal)
   ]

type EmptySpace = PropDict (Group KB.EEmptySpace)
  '[ PropKeyVal KB.EIcon (OwnProp (KB.IconVal "."))
   , PropKeyVal KB.EPos  (OwnProp KB.DerivedPosVal)
   ]

type TestWorld1 = WorldData @TypeLevel
  '[ "..."
   , "#+#"
   , "..."
   ]

type TestGame1 = GameEnvironment
  TestWorld1
  KB.PathToIcon
  KB.PathToPos

  -- Static props for the instantiation from the world data
  '[ EmptySpace
   , SpecificDoor
   , Wall
   ]

  -- Objects (static props placed into the world separately)
  '[ Obj 2 8 GenericDoor
   , Obj 3 9 Wall
   ]
