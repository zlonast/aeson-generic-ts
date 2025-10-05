-- {-# LANGUAGE TemplateHaskell #-}

module BasicExamples where

import           Data.Bool                                (Bool)
import           Data.Function                            (($))
import           Data.Int                                 (Int)
import           Data.Maybe                               (Maybe)
import           Data.Text                                (Text)

import           GHC.Generics                             (Generic)

import           Typescript.Internal.Intermediate.Generic (TypescriptType (..))
import           Typescript.Internal.Intermediate.Lang    (TSComposite (..), TSIntermediate (..), TSPrimitive (..),
                                                           TSStructured (..), TSUnion (..))
-- import           Typescript.TH.GenInstances               (deriveTypescriptTypesRecursively)

data SimpleRecord =
  SimpleRecord
    { f1 :: Int
    , f2 :: Text
    }
    deriving stock (Generic)
    deriving anyclass (TypescriptType)

newtype OneFieldRecord =
 OneFieldRecord
    { onlyField :: Int
    }
    deriving stock (Generic)
    deriving anyclass (TypescriptType)

data ComplexRecord =
  ComplexRecord
    { anIntField    :: Int
    , aTextField    :: Text
    , aUnion        :: SampleUnion
    , aMaybeType    :: Maybe Text
    , aSimpleRecord :: SimpleRecord
    }
    deriving stock (Generic)
    deriving anyclass (TypescriptType)

data SampleUnion = FirstCon Int | SecondCon Text | BoolCon Bool
    deriving stock (Generic)
    -- deriving anyclass (TypescriptType)

instance TypescriptType SampleUnion where
  toTSIntermediate _ =
    TSCompositeType $
      TSStructuredType "SampleUnion" $
        TSUnionLike $
          TSUnion
            [ TSPrimitiveType TSNumber
            , TSPrimitiveType TSString
            , TSPrimitiveType TSBoolean
            ]

-- deriving instance (TypescriptType SampleUnion)

-- $(deriveTypescriptTypesRecursively [ ''SampleUnion ])
