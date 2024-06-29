{-# LANGUAGE DataKinds #-}

module ZP.Testing.TestData where

import ZP.Prelude

import ZP.Domain.Static.Model
import qualified ZP.Assets.KnowledgeBase as KB

import Data.Proxy



type TestIconOwning = OwnVal (KB.IconVal "+")
type TestPropKeyVal = PropKeyVal KB.EIcon TestIconOwning

type TestProp = DerivedProp KB.EIntrinsics KB.AnyProp
  '[ TestPropKeyVal
   ]
  '[]
