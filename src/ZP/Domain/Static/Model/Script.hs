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
data GenericVarDef (lvl :: Level) typeTag where
  GenericVar
    :: StringType lvl             -- ^ Var name
    -> GenericValDef lvl typeTag  -- ^ Default value
    -> GenericVarDef lvl typeTag

-- | Script operation
data ScriptOp (lvl :: Level) where
  DeclareVar :: GenericVarDef lvl typeTag -> ScriptOp lvl

  -- Can be the only MOV instruction
  WriteData
    :: Target lvl typeTag
    -> Source lvl typeTag
    -> ScriptOp lvl

  Invoke
    :: Func lvl typeTag1 typeTag2
    -> Source lvl typeTag1
    -> Target lvl typeTag2
    -> ScriptOp lvl

type ReadData src tgt = WriteData tgt src

-- N.B., Proxy is only needed to satisfy functional dependency
-- that is somehow fails to define typeTag without this workaround.
data Target (lvl :: Level) typeTag where
  ToField :: Proxy typeTag -> EssencePath lvl -> Target lvl typeTag
  ToVar   :: GenericVarDef lvl typeTag -> Target lvl typeTag

-- N.B., Proxy is only needed to satisfy functional dependency
-- that is somehow fails to define typeTag without this workaround.
data Source (lvl :: Level) typeTag where
  FromField :: Proxy typeTag -> EssencePath lvl -> Source lvl typeTag
  FromVar   :: GenericVarDef lvl typeTag -> Source lvl typeTag
  FromConst :: GenericConstDef lvl typeTag -> Source lvl typeTag

-- | Function over a value
data Func (lvl :: Level) typeTag1 typeTag2 where
  NegateF :: Func lvl BoolTag BoolTag

-- | Script type
data CustomScript (lvl :: Level) where
  Script
    :: StringType lvl
    -- ^ Description
    -> [ScriptOp lvl]
    -> CustomScript lvl

-- Predefined var types

-- type IntVar (name :: Symbol) (i :: Nat)
--   = GenericVar @'TypeLevel @IntTag name (IntValue i) IntTag
type IntVar (name :: Symbol) (i :: Nat)
  = GenericVar name (IntValue i)

type BoolVar (name :: Symbol) (b :: Bool)
  = GenericVar name (BoolValue b)

type StringVar (name :: Symbol) (s :: Symbol)
  = GenericVar name (StringValue s)

type PathVar (name :: Symbol) (ss :: [EssenceTL])
  = GenericVar name (PathValue ss)

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

