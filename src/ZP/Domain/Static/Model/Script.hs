{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module ZP.Domain.Static.Model.Script where

import ZP.Prelude
import GHC.TypeLits
import qualified Text.Show as T

import ZP.Domain.Static.Model.Common


-- | Variable definition
data GenericVarDef (lvl :: Level) (tag :: CustomTag) where
  GenericVar
    :: StringType lvl         -- ^ Var name
    -> GenericValDef lvl tag  -- ^ Default value
    -> GenericVarDef lvl tag

-- N.B., Proxy is only needed to satisfy functional dependency
-- that is somehow fails to define tag without this workaround.
data Target (lvl :: Level) (tag :: CustomTag) where
  ToField
    :: Proxy tag -> EssencePath lvl -> Target lvl tag
  ToVar
    :: GenericVarDef lvl tag -> Target lvl tag

-- N.B., Proxy is only needed to satisfy functional dependency
-- that is somehow fails to define tag without this workaround.
data Source (lvl :: Level) (tag :: CustomTag) where
  FromField
    :: Proxy tag -> EssencePath lvl -> Source lvl tag
  FromVar
    :: GenericVarDef lvl tag -> Source lvl tag
  FromConst
    :: GenericConstDef lvl tag -> Source lvl tag

-- | Function over a value
data Func (lvl :: Level) (tag1 :: CustomTag) (tag2 :: CustomTag) where
  NegateF :: Func lvl BoolTag BoolTag

-- | Script operation
data ScriptOp (lvl :: Level) where
  DeclareVar
    :: GenericVarDef lvl (tag :: CustomTag)
    -> ScriptOp lvl

  WriteData
    :: Target lvl (tag :: CustomTag)
    -> Source lvl (tag :: CustomTag)
    -> ScriptOp lvl

  Invoke
    :: Func lvl (tag1 :: CustomTag) (tag2 :: CustomTag)
    -> Source lvl (tag1 :: CustomTag)
    -> Target lvl (tag2 :: CustomTag)
    -> ScriptOp lvl

type ReadData src tgt = 'WriteData tgt src

-- | Script type
data CustomScript (lvl :: Level) where
  Script
    :: StringType lvl
    -- ^ Description
    -> [ScriptOp lvl]
    -> CustomScript lvl

-- Predefined var types

type IntVar (name :: Symbol) (i :: Nat)
  = GenericVar name (IntValue i)

type BoolVar (name :: Symbol) (b :: Bool)
  = GenericVar name (BoolValue b)

type StringVar (name :: Symbol) (s :: Symbol)
  = GenericVar name (StringValue s)

type PathVar (name :: Symbol) (path :: EssencePathTL)
  = GenericVar name (PathValue path)

-- TODO: rest of vars


-- Short definitions

type CustomScriptTL = CustomScript 'TypeLevel
type CustomScriptVL = CustomScript 'ValueLevel

type FuncTL = Func 'TypeLevel
type FuncVL = Func 'ValueLevel

type SourceTL = Source 'TypeLevel
type SourceVL = Source 'ValueLevel

type TargetTL = Target 'TypeLevel
type TargetVL = Target 'ValueLevel

type GenericVarDefTL = GenericVarDef 'TypeLevel
type GenericVarDefVL = GenericVarDef 'ValueLevel

type ScriptOpTL = ScriptOp 'TypeLevel
type ScriptOpVL = ScriptOp 'ValueLevel

