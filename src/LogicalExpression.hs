module LogicalExpression where

data LogicalExpression
  = Lit Literal                                                    -- Literal
  | Var Variable
  | ALoc String                                                    -- Abstract location
  | UnOp UnaryOperator LogicalExpression                           -- Unary operator
  | BinOp LogicalExpression BinaryOperator LogicalExpression       -- Binary operator
  | SubList LogicalExpression LogicalExpression LogicalExpression  -- Sublist (list, start, len)
  | List [LogicalExpression]                                       -- List
  | Set [LogicalExpression]                                        -- Set
  | Exists [(Variable, Maybe Type)] LogicalExpression              -- Existential quantification
  | ForAll [(Variable, Maybe Type)] LogicalExpression              -- Universal quantification

data Literal
  = Undefined
  | Null
  | Empty
  | Bool Bool
  | Int Int
  | Num Float
  | String String
  | Loc String         -- Object location
  | Type Type
  | LitList [Literal]  -- List of literals

data UnaryOperator
  = IntNeg
  | NumNeg
  | Not
  | BitwiseNot
  | TypeOf
  | Head
  | Tail
  | ListLen
  | ListRev
  | ToList      -- From Set
  | StrLen
  | ToInt       -- From Num
  | ToNum       -- From Int

data BinaryOperator
  = Equal
  | IntLt
  | IntLte
  | IntPlus
  | IntMinus
  | IntTimes
  | IntDiv
  | IntMod
  | NumLt
  | NumLte
  | NumPlus
  | NumMinus
  | NumTimes
  | NumDiv
  | NumMod
  | And
  | Or
  | Impl
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | ListNth
  | ListRepeat
  | StrConcat
  | StrNth
  | SetDiff
  | SetMem
  | SetSub

data Type
  = UndefinedType
  | NullType
  | EmptyType
  | NoneType
  | BoolType
  | IntType
  | NumType
  | StringType
  | ObjectType
  | ListType
  | TypeType
  | SetType
  deriving (Eq, Show)

data Variable
  = LVar String | PVar String
  deriving (Eq, Ord)

var :: Variable -> String
var (LVar x) = x
var (PVar x) = x

-- TODO: This differs from Gillian implementation.
locFromName :: String -> LogicalExpression
locFromName = ALoc
