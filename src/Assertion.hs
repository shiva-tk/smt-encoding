module Assertion where

import LogicalExpression (LogicalExpression, Type)

data Assertion p
  = Emp
  | Pred String [LogicalExpression]
  | Pure LogicalExpression
  | Types [(LogicalExpression, Type)]  -- TODO: Remove typing assertions
  | CorePred p
