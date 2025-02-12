{-# LANGUAGE FunctionalDependencies #-}

module SymbolicMemory where

import Assertion (Assertion)


class SymbolicMemory m p | m -> p where
  assertions :: m -> [Assertion p]
