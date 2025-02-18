module TypeEnv where

import           Assertion         (Assertion (Types))
import           Control.Monad     (foldM)
import           Data.List         (singleton)
import           Data.Map          (Map, insert, delete, (!?), toList, filterWithKey)
import           Data.Tuple.Extra  (first)
import           LogicalExpression (BinaryOperator (..), Literal (..),
                                    LogicalExpression (..), Type (..),
                                    UnaryOperator (..), Variable)

class Typable t where
  typeOf :: t -> Type

instance Typable Literal where
  typeOf Undefined   = UndefinedType
  typeOf Null        = NullType
  typeOf Empty       = EmptyType
  typeOf (Bool _)    = BoolType
  typeOf (Int _)     = IntType
  typeOf (Num _)     = NumType
  typeOf (String _)  = StringType
  typeOf (Loc _)     = ObjectType
  typeOf (Type _)    = TypeType
  typeOf (LitList _) = ListType

type TypeEnv = Map Variable Type

assertions :: TypeEnv -> [Assertion p]
assertions = singleton . Types . map (first Var) . toList

-- Extends a typing environment with the inferred types from a logical expression
inferTypes :: TypeEnv -> LogicalExpression -> Maybe TypeEnv
inferTypes gamma (Lit _)   = Just gamma
inferTypes gamma (Var _)   = Just gamma
inferTypes gamma (ALoc _)  = Just gamma
inferTypes gamma (List es) = foldM inferTypes gamma es
inferTypes gamma (Set es)  = foldM inferTypes gamma es
-- Most sub-cases not actually implemented in Gillian: https://github.com/GillianPlatform/Gillian/blob/d52b2ae273e5adc36c3a48688a25553b41efcf59/GillianCore/engine/FOLogic/typing.ml#L252
inferTypes gamma (UnOp op e) = case op of
  IntNeg     -> expect IntType
  NumNeg     -> expect NumType
  Not        -> expect BoolType
  BitwiseNot -> expect IntType
  Head       -> expect ListType
  Tail       -> expect ListType
  ListLen    -> expect ListType
  ListRev    -> expect ListType
  ToList     -> expect SetType
  StrLen     -> expect StringType
  ToInt      -> expect NumType
  ToNum      -> expect IntType
  _          -> Just gamma  -- No new information gained: Currently this is what is happening in most cases in Gillian.
  where
    -- Extend the typing environment, given the expected type of expression e
    expect = extend gamma e
inferTypes gamma (BinOp e op e') = case op of
  IntLt      -> expect IntType IntType
  IntLte     -> expect IntType IntType
  IntPlus    -> expect IntType IntType
  IntMinus   -> expect IntType IntType
  IntTimes   -> expect IntType IntType
  IntDiv     -> expect IntType IntType
  IntMod     -> expect IntType IntType
  NumLt      -> expect NumType NumType
  NumLte     -> expect NumType NumType
  NumPlus    -> expect NumType NumType
  NumMinus   -> expect NumType NumType
  NumTimes   -> expect NumType NumType
  NumDiv     -> expect NumType NumType
  NumMod     -> expect NumType NumType
  And        -> expect BoolType BoolType
  Or         -> expect BoolType BoolType
  Impl       -> expect BoolType BoolType
  ListNth    -> expect ListType IntType
  ListRepeat -> expect ListType IntType
  StrConcat  -> expect StringType StringType
  StrNth     -> expect StringType IntType
  SetDiff    -> expect SetType SetType
  SetMem     -> expect' SetType  -- Cannot infer type of e'
  SetSub     -> expect SetType SetType
  _          -> Just gamma -- No new information gained
  where
    -- Extend the typing environment, given the expected type of expression e / e' respectively
    expect t t' = case expect' t of
      Just gamma' -> extend gamma' e' t'
      Nothing     -> Nothing
    expect' t = extend gamma e t

-- Given knowledge of the type of a logical expression, extend the typing environment
extend :: TypeEnv -> LogicalExpression -> Type -> Maybe TypeEnv
extend gamma (Lit l) t
  | typeOf l == t = Just gamma
  | otherwise     = Nothing
extend gamma (Var v) t = case mt' of
  Just t' -> if t == t' then Just gamma else Nothing
  Nothing -> Just gamma'
  where mt'    = gamma !? v
        gamma' = insert v t gamma
extend gamma (ALoc _) t
  | t == ObjectType = Just gamma
  | otherwise       = Nothing
extend gamma (UnOp op e) t = case op of
  IntNeg     -> expect IntType IntType
  NumNeg     -> expect NumType NumType
  Not        -> expect BoolType BoolType
  BitwiseNot -> expect IntType IntType
  TypeOf     -> if t == TypeType then Just gamma else Nothing
  Head       -> extend gamma e ListType
  Tail       -> expect ListType ListType
  ListLen    -> expect ListType IntType
  ListRev    -> expect ListType ListType
  ToList     -> expect SetType ListType
  StrLen     -> expect StringType IntType
  ToInt      -> expect NumType IntType
  ToNum      -> expect NumType IntType
  where
    -- Expecting a u' -> u function
    expect :: Type -> Type -> Maybe TypeEnv
    expect u' u
      | u == t    = extend gamma e u' -- Return types align, check input type matches what we expect
      | otherwise = Nothing           -- Return types don't align
extend gamma (BinOp e1 op e2) t = case op of
  Equal      -> expect Nothing Nothing (Just BoolType)
  IntLt      -> expect (Just IntType) (Just IntType) (Just BoolType)
  IntLte     -> expect (Just IntType) (Just IntType) (Just BoolType)
  IntPlus    -> expect (Just IntType) (Just IntType) (Just BoolType)
  IntMinus   -> expect (Just IntType) (Just IntType) (Just BoolType)
  IntTimes   -> expect (Just IntType) (Just IntType) (Just BoolType)
  IntDiv     -> expect (Just IntType) (Just IntType) (Just BoolType)
  IntMod     -> expect (Just IntType) (Just IntType) (Just BoolType)
  NumLt      -> expect (Just NumType) (Just NumType) (Just NumType)
  NumLte     -> expect (Just NumType) (Just NumType) (Just NumType)
  NumPlus    -> expect (Just NumType) (Just NumType) (Just NumType)
  NumMinus   -> expect (Just NumType) (Just NumType) (Just NumType)
  NumTimes   -> expect (Just NumType) (Just NumType) (Just NumType)
  NumDiv     -> expect (Just NumType) (Just NumType) (Just NumType)
  NumMod     -> expect (Just NumType) (Just NumType) (Just NumType)
  And        -> expect (Just BoolType) (Just BoolType) (Just BoolType)
  Or         -> expect (Just BoolType) (Just BoolType) (Just BoolType)
  Impl       -> expect (Just BoolType) (Just BoolType) (Just BoolType)
  BitwiseAnd -> expect (Just IntType) (Just IntType) (Just BoolType)
  BitwiseOr  -> expect (Just IntType) (Just IntType) (Just BoolType)
  BitwiseXor -> expect (Just IntType) (Just IntType) (Just BoolType)
  ListNth    -> expect (Just ListType) (Just IntType) (Nothing)
  ListRepeat -> expect (Just ListType) (Just IntType) (Just ListType)
  StrConcat  -> expect (Just StringType) (Just StringType) (Just StringType)
  StrNth     -> expect (Just StringType) (Just IntType) (Nothing)  -- This seems unusual
  SetDiff    -> expect (Just SetType) (Just SetType) (Just SetType)
  SetMem     -> expect (Nothing) (Just SetType) (Just BoolType)
  SetSub     -> expect (Just SetType) (Just SetType) (Just BoolType)
  where
    -- Expecting a t1 -> t2 -> t3 function
    -- If None is provided, no type restrictions
    expect :: Maybe Type -> Maybe Type -> Maybe Type -> Maybe TypeEnv
    expect mt1 mt2 mt3 = do
      t1       <- mt1
      gamma'   <- extend gamma e1 t1
      t2       <- mt2
      gamma''  <- extend gamma' e2 t2
      t3       <- mt3
      gamma''' <- if t == t3 then Just gamma'' else Nothing
      return gamma'''
extend gamma (SubList l s e) t = do
  gamma'    <- extend gamma l ListType
  gamma''   <- extend gamma' s IntType
  gamma'''  <- extend gamma'' e IntType
  gamma'''' <- if t == ListType then Just gamma''' else Nothing
  return gamma''''
extend gamma (List _) t
  | t == ListType = Just gamma
  | otherwise     = Nothing
extend gamma (Set _) t
  | t == SetType  = Just gamma
  | otherwise     = Nothing
extend gamma (Exists vs e) t = extendQuantified gamma vs e t
extend gamma (ForAll vs e) t = extendQuantified gamma vs e t

-- Not exactly what Gillian is doing, since Gillian is differentiating new_gamma from gamma: https://github.com/GillianPlatform/Gillian/blob/d52b2ae273e5adc36c3a48688a25553b41efcf59/GillianCore/engine/FOLogic/typing.ml#L165
extendQuantified :: TypeEnv -> [(Variable, Maybe Type)] -> LogicalExpression -> Type -> Maybe TypeEnv
extendQuantified gamma vs e t
  | t == BoolType = do
      gamma'' <- extend gamma' e BoolType
      -- Remove quantified vars
      return $ filterWithKey (const . not . isQuantified) gamma''
  | otherwise     = Nothing
  where
    applyUpdate gamma (v, Nothing) = delete v gamma
    applyUpdate gamma (v, Just t') = insert v t' gamma
    -- For quantified variables:
    --   If type is known, update the type in the environment
    --   Otherwise, delete the variable from the typing environment
    gamma'         = foldl applyUpdate gamma vs
    isQuantified v = any ((== v) . fst) vs
