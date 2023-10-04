{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module ZP.Typed.KnowledgeBase.Common where

import ZP.Typed.Model.Property
import ZP.Typed.KnowledgeBase.Essences

import GHC.TypeLits


type HPVal range  = PropVal (EssRoot EHP) (IntRangeValDef range)
type PosVal x y   = PropVal (EssRoot EPos) (PairValDef (IntValDef x) (IntValDef y))
type PosConst x y = PropConst (EssRoot EPos) (PairValDef (IntValDef x) (IntValDef y))
