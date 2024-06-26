{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.Surroundings where

import ZP.Prelude

import ZP.Domain.Static.Model
import ZP.Assets.KnowledgeBase.Essences
import ZP.Assets.KnowledgeBase.Common

import GHC.TypeLits


-- Templates for static objects on the map (without pos prop)

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
