module Main (main) where

import           Data.Function        (($))
import           Data.Semigroup       (Semigroup (..))

import           GHC.Tuple            (Unit)

import           System.FilePath.Glob (compile, globDir1)
import           System.IO            (IO)

import           Test.DocTest         (doctest)

main :: IO Unit
main = do
  let options =
        [ "-XConstraintKinds"
        , "-XDataKinds"
        , "-XDeriveGeneric"
        , "-XFlexibleContexts"
        , "-XFlexibleInstances"
        , "-XKindSignatures"
        , "-XMultiParamTypeClasses"
        , "-XOverloadedStrings"
        , "-XRecordWildCards"
        , "-XScopedTypeVariables"
        , "-XStandaloneDeriving"
        , "-XTypeFamilies"
        , "-XUndecidableInstances"
        , "-XDerivingStrategies"
        , "-XBlockArguments"
        , "-XDeriveAnyClass"
        , "-XDerivingVia"
        , "-XGeneralizedNewtypeDeriving"
        , "-XNoListTuplePuns"
        , "-XNoStarIsType"
        , "-XOverloadedLabels"
        , "-XOverloadedRecordDot"
        , "-XPackageImports"
        , "-XStrictData"
        , "-XTypeOperators"
        , "-XViewPatterns"
        , "-XDefaultSignatures"
        , "-XGHC2024"
        ]

  paths <- globDir1 (compile "**/*.hs") "src"
  doctest $ options <> paths
