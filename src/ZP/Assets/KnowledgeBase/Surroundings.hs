{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.Surroundings where

import ZP.Domain.Static.Model
import ZP.Domain.Hardcode.KnowledgeBase
import ZP.Assets.KnowledgeBase.Essences
import ZP.Assets.KnowledgeBase.Common

import Prelude (Bool(..))
import GHC.TypeLits

-- Templates for static objects on the map (without pos prop)

type Wall = PropDict (EssStaticRoot EWall)
  '[ PropKeyVal EIcon (OwnProp (IconVal "#"))
   ]

type EmptySpace = PropDict (EssStaticRoot EEmptySpace)
  '[ PropKeyVal EIcon (OwnProp (IconVal "."))
   ]