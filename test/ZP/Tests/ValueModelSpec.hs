{-# LANGUAGE DataKinds #-}

module ZP.Tests.ValueModelSpec where

import ZP.Prelude

import ZP.Domain.Static.Query
import ZP.Domain.Static.Model
import ZP.Domain.Static.Materialization
import qualified ZP.Domain.Static.Description as Descr
import qualified ZP.Assets.KnowledgeBase.Essences as KB
import qualified ZP.Assets.KnowledgeBase.Common as KB

import ZP.Testing.TestData
import ZP.System.Debug
import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map



type ESomeProp = Ess @TypeLevel "some prop"
type ESomeAbstractProp = Ess @TypeLevel "some abstract prop"
type EHPVal = Ess @TypeLevel "hp val"
type ETest = Ess @TypeLevel "test script"
type EGenericPos = Ess @TypeLevel "generic pos"


type HPVal hp = IntValue hp
type HPVar = IntVar "hp var" 0
type NameVar = StringVar "name var" "John Doe"

type GenericPos    = TagProp (TagGroup EGenericPos)
type PosVal x y    = IntPairValue x y
type PosTagVal x y = TagPropertyValue GenericPos (PosVal x y)
type PosTagVar x y = TagPropertyVar "pos var" GenericPos (PosVal x y)

type TestScript = 'Script @'TypeLevel "test script"
  '[ DeclareVar HPVar
   , DeclareVar (PosTagVar 1 1)

   , WriteData (ToVar HPVar)
               (FromConst (IntConst 30))

   , ReadData (FromVar HPVar)
              (ToField 'Proxy (RelPath '[ EHPVal ]))

  -- Won't compile, type mismatch:
  --  , WriteData (ToVar HPVar)
  --              (FromVar NameVar)

   ]


data Person (lvl :: Level) where
  Person :: StringType lvl -> StringType lvl -> Person lvl

data PersonValueHolder (lvl :: Level) (tag :: CustomTag)
  = PVH
    (Person lvl)
    (GenericValDef lvl tag)

type UserTypeScript = 'Script @'TypeLevel "user type usage script"
  '[ DeclareVar HPVar

   , WriteData (ToVar HPVar)
               (FromConst (IntConst 30))
   ]




type SomeAbstractProp = AbstractProp (Group ESomeAbstractProp) '[] '[]

type SomeProp = DerivedProp ESomeProp SomeAbstractProp
  '[ PropKeyVal EHPVal (OwnVal (HPVal 10))
   ]
  '[ PropScript ETest TestScript
   ]


spec :: Spec
spec = do
  describe "Value model" $ do
    it "Common value type materialization" $ do
      sEnv <- makeSEnv DebugDisabled

      _ <- sMat' sEnv () $ Proxy @SomeProp

      statProps <- readIORef $ seStaticPropertiesRef sEnv
      statEsss  <- readIORef $ seStaticEssencesRef sEnv

      -- print $ "Stat props: " <> show (Map.keys statProps)
      -- print $ "Stat essences: " <> show (Map.keys statEsss)

      length statProps `shouldBe` 2

      let (_, prop) = fromJust $ Map.lookup (StaticPropertyId 1) statProps

      -- Descr.printDescription prop

      case prop of
        PropDict group props scripts -> do
          let (ess, sId) = getComboId group
          sId `shouldBe` StaticPropertyId 1
          length scripts `shouldBe` 1
          length props `shouldBe` 1
          ess `shouldBe` Ess "some prop"
          Map.member ess statEsss `shouldBe` True
        _ -> error "invalid materialization result"
