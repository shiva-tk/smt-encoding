module SymbolicStore where

import           Assertion         (Assertion (Pure))
import           LogicalExpression (BinaryOperator (Equal),
                                    LogicalExpression (Var, BinOp),
                                    Variable (PVar))

-- Mapping from program variables to logical expressions
type SymbolicStore = [(String, LogicalExpression)]

assertions :: SymbolicStore -> [Assertion p]
assertions = map (\(x, v) -> Pure $ BinOp (Var $ PVar x) Equal v)
