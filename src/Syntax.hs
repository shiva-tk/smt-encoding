module Syntax where

data Typ = Nat | Bool | Str | Null | List | DataType

data Val = NatVal Int | BoolVal Bool | StrVal String | NullVal | ListVals [Val]

type PVar = String

type LVar = String

data LExp =
  Val Val
  | PVar PVar        -- Program variables
  | LVar LVar        -- Logical variables
  | (:+:)  LExp LExp -- Addition
  | (:-:)  LExp LExp -- Subtraction
  | (:.:)  LExp LExp -- Multiplication
  | (:/:)  LExp LExp -- Division
  | (:=:)  LExp LExp -- Equality
  | (:<:)  LExp LExp -- Less than
  | (:¬:)  LExp LExp -- Negation
  | (:/\:) LExp LExp -- Conjunction
  | (:::)  LExp LExp -- List cons
  | TypeOf LExp Typ  -- Type of

data Asrt r =
  Pure LExp                  -- Pure formulae
  | T                        -- True
  | (:=>:) (Asrt r) (Asrt r) -- Implication
  | (:\/:) (Asrt r) (Asrt r) -- Disjunction
  | Exists [String] (Asrt r) -- ∃ x, y, z. A
  | Emp                      -- Empty memory assertion
  | (:*:)  (Asrt r) (Asrt r) -- Separating conjunction
  | Resource r [LExp]        -- Resource assertion
  | Pred Pred

type Pred = (String, [LExp])
