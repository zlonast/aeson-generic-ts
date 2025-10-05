module Typescript.Internal.Intermediate.Lang where

import           Data.Eq        (Eq)
import           Data.Function  (($))
import           Data.List      (List)
import           Data.Semigroup (Semigroup (..))
import           Data.Text      (Text)

import           Text.Show      (Show)

{-
   MASTER TYPE
-}
data TSIntermediate f =
    TSPrimitiveType TSPrimitive
  | TSCompositeType (TSComposite f)

{-
  Typescript Primitives. Have a default rep
-}
data TSPrimitive = TSNumber | TSString | TSBoolean | TSVoid
    deriving stock (Eq, Show)

{-
  Composite Types
-}
data TSComposite f =
    TSCollectionRef (TSCollection f)
  | TSOptionRef (TSOption f)
  | TSStructuredType Text (TSStructured f)

newtype TSCollection f = TSCollection (TSIntermediate f)

newtype TSOption f = TSOption (TSIntermediate f)

newtype TSUnion f = TSUnion (List (TSIntermediate f))

instance Semigroup (TSUnion f) where
  (TSUnion l1) <> (TSUnion l2) = TSUnion $ l1 <> l2

{-
  Typescript "Data types", which are largely structural but can also be transformed into classes.
-}
data TSStructured f = TSRecordLike (TSRecord f) | TSUnionLike (TSUnion f)

newtype TSRecord f = TSRecord (List (TSField f))

data TSField f = TSField
  { fieldName :: FieldName
  , fieldType :: TSIntermediate f
  }

newtype FieldName = FieldName Text
