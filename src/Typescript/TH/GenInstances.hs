-- | Quick helper functions for (recursively) generating typescript instances using template haskell
{-# LANGUAGE TemplateHaskell #-}

module Typescript.TH.GenInstances where

import           Control.Monad                            (mapM)

import           Data.Bool                                (Bool (..))
import           Data.Function                            (const)
import           Data.Functor                             ((<$>))
import           Data.List                                (List, concat)

import           Language.Haskell.TH                      (Dec, Name, Q, conT)
import           Language.Haskell.TH.ReifyMany            (reifyManyWithoutInstances)

import           Typescript.Internal.Intermediate.Generic (TypescriptType)

deriveTypescriptTypesRecursively :: List Name -> Q (List Dec)
deriveTypescriptTypesRecursively nms = do
  names <- reifyManyWithoutInstances ''TypescriptType nms (const True)
  concat <$> mapM deriveTSInstance names

deriveTSInstance :: Name -> Q (List Dec)
deriveTSInstance nm = do
  [d|deriving instance TypescriptType $(conT nm)|]
