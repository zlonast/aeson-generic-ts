module Typescript.Internal.Flavors.Vanilla where

import           Typescript.Internal.Intermediate.Lang         (TSComposite (..), TSStructured (..))
import           Typescript.Internal.Output.Foreign.Class      (IsForeignType (..))
import           Typescript.Internal.Output.Foreign.TSDefaults (defaultForeignArray, defaultForeignUnion, defaultOption,
                                                                mkTSInterface)

data Vanilla

instance IsForeignType (TSComposite Vanilla) where
  toForeignType (TSCollectionRef tsCollection) =
    defaultForeignArray tsCollection
  toForeignType (TSOptionRef tsOption) = defaultOption tsOption
  toForeignType (TSStructuredType typeName tsStructure) =
    case tsStructure of
      TSUnionLike tsUnion -> defaultForeignUnion typeName tsUnion
      TSRecordLike tsData -> mkTSInterface typeName tsData
