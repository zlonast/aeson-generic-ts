module Typescript.Internal.Output.PrintForeign where

import           Data.Function                            (($))
import           Data.Proxy                               (Proxy)
import           Data.Text                                (Text)

import           Typescript.Internal.Intermediate.Generic (TypescriptType (..))
import           Typescript.Internal.Intermediate.Lang    (TSIntermediate)
import           Typescript.Internal.Output.Foreign.Class (ForeignType (..), IsForeignType (..))

{-| Core function for outputting typescript

Simple example using the `Vanilla` flavor

-}
-- |
-- >>> import Typescript.Internal.Flavors.Vanilla
-- >>> import Data.Proxy
-- >>> declaration $ foreignTypescript (Proxy :: Proxy Vanilla) (Proxy :: Proxy Int)
-- "number"
foreignTypescript
  :: (TypescriptType hsType, IsForeignType (TSIntermediate flavor))
  => Proxy flavor -> Proxy hsType -> ForeignType
foreignTypescript pFlavor tsType' = toForeignType $ toTSFlavor pFlavor tsType'

mkTypescriptDeclaration
  :: (TypescriptType hsType, IsForeignType (TSIntermediate flavor))
  => Proxy flavor -> Proxy hsType -> Text
mkTypescriptDeclaration pFlavor tsType' =
  declaration $ foreignTypescript pFlavor tsType'

toTSFlavor :: (TypescriptType hsType)
  => Proxy flavor -> Proxy hsType -> TSIntermediate flavor
toTSFlavor _ = toTSIntermediate
