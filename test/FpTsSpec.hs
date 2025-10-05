-- {-# LANGUAGE TemplateHaskell #-}

module FpTsSpec (spec) where

import           Data.Function                            (($))
import           Data.Maybe                               (Maybe)
import           Data.Proxy                               (Proxy (..))
import           Data.Text                                (Text)

import           GHC.Generics                             (Generic)

import           Test.Hspec                               (Spec, describe, it, shouldBe)

import           Typescript.Internal.Flavors.FpTs         (FpTs)
import           Typescript.Internal.Intermediate.Generic (TypescriptType)
import           Typescript.Internal.Output.PrintForeign  (mkTypescriptDeclaration)
-- import           Typescript.TH.GenInstances               (deriveTypescriptTypesRecursively)

newtype AnOption = AnOption (Maybe Text)
    deriving stock (Generic)
    deriving anyclass (TypescriptType)

-- deriving anyclass instance (TypescriptType AnOption)

newtype F =
  F { f1 :: AnOption }
    deriving stock (Generic)
    deriving anyclass (TypescriptType)

-- deriving anyclass instance (TypescriptType F)

-- $(deriveTypescriptTypesRecursively [ ''F ])

spec :: Spec
spec = describe "option_type" $ do
    it "handles_a_simple_option" $ do
        let knownSolution = "interface AnOption { \n   : Option<string>\n}"
        printFpTs (Proxy :: Proxy AnOption) `shouldBe` knownSolution

    it "handles_a_simple_option" $ do
        let answer = "interface F { \n  f1 : AnOption\n}"
        printFpTs (Proxy :: Proxy F) `shouldBe` answer

printFpTs :: (TypescriptType a) => Proxy a -> Text
printFpTs = mkTypescriptDeclaration (Proxy :: Proxy FpTs)
