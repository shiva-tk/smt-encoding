{-# LANGUAGE MultiParamTypeClasses #-}

module WislSymbolicMemory where

import           Assertion         (Assertion (CorePred))
import           LogicalExpression (LogicalExpression (Lit), locFromName, Literal (Int))
import SymbolicMemory (SymbolicMemory (assertions))

data Block
  = Deallocated
  | Allocated {offsets :: Offsets, bound :: Int}

type Offsets = [(LogicalExpression, LogicalExpression)]

newtype WislHeap = WislHeap [(String, Block)]

data WislPreds
  = Cell LogicalExpression LogicalExpression LogicalExpression
  | Bound LogicalExpression LogicalExpression
  | Freed LogicalExpression

blockAssertions :: String -> Block -> [Assertion WislPreds]
blockAssertions l Deallocated     = [CorePred $ Freed $ locFromName l]
blockAssertions l (Allocated os b) = boundAssertion : dataAssertions
  where dataAssertions  = map (\(o, v) -> CorePred $ Cell (locFromName l) o v) os
        boundAssertion  = CorePred $ Bound (locFromName l) (Lit $ Int b)

instance SymbolicMemory WislHeap WislPreds where
  assertions (WislHeap h) = concatMap (uncurry blockAssertions) h
