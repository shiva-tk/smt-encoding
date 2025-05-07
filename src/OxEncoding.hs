module OxEncoding where

import SymbolicState
import SimpleSMT
import Syntax (LExp)
import TypeEnvironment

encodeSAT :: SMemory m => TypeEnv -> SState m -> SExpr
encodeSAT = undefined -- TODO

encodeEntailment :: SMemory m => TypeEnv -> SState m -> LExp -> SExpr
encodeEntailment = undefined -- TODO

encodeLExp ::TypeEnv -> LExp -> SExpr
encodeLExp = undefined -- TODO
