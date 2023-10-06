{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.Common where

import ZP.Domain.Static.Model
import ZP.Assets.KnowledgeBase.Essences

import GHC.TypeLits


type HPVal hp     = PropVal (EssRoot EHP) (IntValDef hp)
type PosVal x y   = PropVal (EssRoot EPos) (PairValDef (IntValDef x) (IntValDef y))
type PosConst x y = PropConst (EssRoot EPos) (PairValDef (IntValDef x) (IntValDef y))
