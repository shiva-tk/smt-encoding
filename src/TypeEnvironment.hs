module TypeEnvironment where

import qualified Data.Map as Map
import Syntax

type TypeEnv = Map.Map String Typ
