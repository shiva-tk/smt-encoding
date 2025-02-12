module PathConstraint where

import           Assertion         (Assertion (Pure))
import           LogicalExpression (LogicalExpression)

type PathConstraint = [LogicalExpression]

assertions :: PathConstraint -> [Assertion p]
assertions = map Pure
