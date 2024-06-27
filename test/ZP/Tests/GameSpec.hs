{-# LANGUAGE DataKinds #-}


{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


module ZP.Tests.GameSpec where

import ZP.Prelude

import qualified ZP.Assets.KnowledgeBase as KB
import ZP.Domain.Dynamic.Model
import ZP.Domain.Static.Materialization
import ZP.Domain.Dynamic.Instantiation
import ZP.System.Debug

import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map


import GHC.TypeLits
import qualified GHC.TypeLits as TL

-- type family Replace (s :: Symbol) (c :: Symbol) (pos :: Nat) :: Symbol where
--     -- Replace s _ (-1) = TypeError (TL.Text "Negative position!")
--     Replace "" _ _   = TypeError (TL.Text "Position is out of bounds!")
--     Replace (x ': xs) c 0 = AppendSymbol c xs
--     Replace (x ': xs) c n = AppendSymbol (x ': "") (Replace xs c (n - 1))


-- type MyString = "ABC"
-- type MyChar = "X"
-- type Position = 1
-- -- type MyNewString = Replace MyString MyChar Position -- will be "AXC"



-- -- Convert a Symbol to a list of characters
-- type family SymbolToList (s :: Symbol) :: [Nat] where
--     SymbolToList "" = '[]
--     SymbolToList (c ': cs) = CharToNat c ': SymbolToList cs

-- -- Convert a Char back to a Symbol
-- type family CharToSymbol (c :: Nat) :: Symbol where
--     CharToSymbol x = NatToChar x ': ""

-- -- Convert a list of characters back to a Symbol
-- type family ListToSymbol (cs :: [Nat]) :: Symbol where
--     ListToSymbol '[] = ""
--     ListToSymbol (c ': cs) = AppendSymbol (CharToSymbol c) (ListToSymbol cs)

-- data World' where
--   WorldData' :: [Symbol] -> World'


-- data Macro' where
--   M :: [Macro] -> MacroGame

-- data Macro' where
--   Displace :: Nat -> Nat ->  -> Macro



spec :: Spec
spec = do
  describe "Game tests" $ do

    xit "World items displacement test" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      1 `shouldBe` 2


    xit "Effect triggering test" $ do
      (sEnv, dEnv) <- makeEnvs DebugDisabled

      1 `shouldBe` 2

