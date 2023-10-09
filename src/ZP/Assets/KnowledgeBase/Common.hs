{-# LANGUAGE DataKinds #-}

module ZP.Assets.KnowledgeBase.Common where

import ZP.Domain.Static.Model
import ZP.Assets.KnowledgeBase.Essences

import GHC.TypeLits


type HPVal hp     = PropVal (EssStaticRoot EHP) (IntValue hp)
type PosVal x y   = PropVal (EssStaticRoot EPos) (PairValue (IntValue x) (IntValue y))
type PosConst x y = PropConst (EssStaticRoot EPos) (PairValue (IntValue x) (IntValue y))
