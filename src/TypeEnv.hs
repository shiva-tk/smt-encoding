module TypeEnv where

import           Assertion         (Assertion (Types))
import           Data.List         (singleton)
import           Data.Tuple.Extra
import           LogicalExpression (LogicalExpression (Var), Type, Variable)

type TypeEnv = [(Variable, Type)]

assertions :: TypeEnv -> [Assertion p]
assertions = singleton . Types . map (first Var)
