module SymbolicState where

import Syntax
import qualified Data.Map as Map

data SState m = SState
  { store         :: SStore
  , preds         :: [Pred]
  , memory        :: m
  , pathCondition :: LExp
  }

type SStore = Map.Map PVar LExp

class SMemory m where
  todo :: m -> m
