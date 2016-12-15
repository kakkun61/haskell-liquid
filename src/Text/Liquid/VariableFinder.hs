{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Text.Liquid.VariableFinder where

import           Control.Arrow
import           Control.Monad.State.Lazy
import qualified Data.List                as L
import qualified Data.List.NonEmpty       as NEL
import           Text.Liquid.Types


-- | Allowed types
data VType
  = VString
  | VNumber
  | VStringOrNumber
  | VBool
  | VAny
  deriving (Eq, Show)

-- | Map findAllVariables across terms from parsed template and concatenate variable with inferred type
findAllVariables
  :: [Expr]
  -> [(JsonVarPath, VType)]
findAllVariables exps =
  L.nub . join $ flip runStateT VStringOrNumber . findVariables mzero <$> exps

-- | Find all the variables in a node's children, with a type scope
findVariables
  :: StateT VType [] JsonVarPath
  -> Expr
  -> StateT VType [] JsonVarPath
findVariables vs e = case e of
  (Variable v)        -> return v `mplus` vs
  (Equal l r)         -> findVariables mzero l `mplus`
                         findVariables mzero r `mplus`
                         vs
  (NotEqual l r)      -> findVariables mzero l `mplus`
                         findVariables mzero r `mplus`
                         vs
  (GtEqual l r)       -> put VNumber >> findVariables mzero l `mplus`
                                        findVariables mzero r `mplus`
                                        vs
  (LtEqual l r)       -> put VNumber >> findVariables mzero l `mplus`
                                        findVariables mzero r `mplus`
                                        vs
  (Gt l r)            -> put VNumber >> findVariables mzero l `mplus`
                                        findVariables mzero r `mplus`
                                        vs
  (Lt l r)            -> put VNumber >> findVariables mzero l `mplus`
                                        findVariables mzero r `mplus`
                                        vs
  (Or l r)            -> findVariables mzero l `mplus`
                         findVariables mzero r `mplus`
                         vs
  (And l r)           -> findVariables mzero l `mplus`
                         findVariables mzero r `mplus`
                         vs
  (Contains l r)      -> findVariables mzero l `mplus`
                         findVariables mzero r `mplus`
                         vs
  (Truthy t)          -> put VBool >> findVariables vs t
  (IfClause i)        -> findVariables vs i
  (IfKeyClause i)     -> findVariables vs i
  (ElsIfClause i)     -> findVariables vs i
  (Filter f xs)       -> put VString >> findVariables mzero f             `mplus`
                                        msum (findVariables mzero <$> xs) `mplus`
                                        vs
  (Output o)          -> findVariables vs o
  (TrueStatements xs) -> msum (findVariables mzero <$> xs) `mplus` vs
  (IfLogic l r)       -> findVariables mzero l `mplus`
                         findVariables mzero r `mplus`
                         vs
  (CaseLogic c xts)   -> let (ls, rs) = unzip xts
                         in findVariables mzero c             `mplus`
                            msum (findVariables mzero <$> ls) `mplus`
                            msum (findVariables mzero <$> rs) `mplus`
                            vs
  _                   -> vs

-- | Find all context variables and add a sample filter.
--   Designed to simulate required templates for aggregate contexts - see JsonTools
makeAggregate
  :: Expr
  -- ^ Aggregate sample filter to add
  -> VarIndex
  -- ^ Prefix to filter on - do not aggregate this prefix
  -> [Expr]
  -- ^ Parsed template to make `aggregate` style
  -> [Expr]
makeAggregate af pf xs =
  aggregateElem af pf <$> xs

aggregateElem
  :: Expr
  -- ^ Aggregate sample filter to add
  -> VarIndex
  -- ^ Prefix to filter on - do not aggregate this prefix
  -> Expr
  -- ^ Expression under modification
  -> Expr
aggregateElem _  _ Noop                             = Noop
aggregateElem _  _ r@(RawText _)                    = r
aggregateElem _  _ n@(Num _)                        = n
aggregateElem _  _ v@(Variable _)                   = v
aggregateElem _  _ q@(QuoteString _)                = q
aggregateElem af pf (Equal l r)                     =
  Equal (aggregateElem af pf l) (aggregateElem af pf r)
aggregateElem af pf (NotEqual l r)                  =
  NotEqual (aggregateElem af pf l) (aggregateElem af pf r)
aggregateElem af pf (GtEqual l r)                   =
  GtEqual (aggregateElem af pf l) (aggregateElem af pf r)
aggregateElem af pf (LtEqual l r)                   =
  LtEqual (aggregateElem af pf l) (aggregateElem af pf r)
aggregateElem af pf (Gt l r)                        =
  Gt (aggregateElem af pf l) (aggregateElem af pf r)
aggregateElem af pf (Lt l r)                        =
  Lt (aggregateElem af pf l) (aggregateElem af pf r)
aggregateElem af pf (Or l r)                        =
  Or (aggregateElem af pf l) (aggregateElem af pf r)
aggregateElem af pf (And l r)                       =
  And (aggregateElem af pf l) (aggregateElem af pf r)
aggregateElem af pf (Contains l r)                  =
  Contains (aggregateElem af pf l) (aggregateElem af pf r)
aggregateElem af pf (Truthy x)                      =
  Truthy (aggregateElem af pf x)
aggregateElem af pf (IfClause x)                    =
  IfClause (aggregateElem af pf x)
aggregateElem af pf (IfKeyClause x)                 =
  IfKeyClause (aggregateElem af pf x)
aggregateElem af pf (ElsIfClause x)                 =
  ElsIfClause (aggregateElem af pf x)
aggregateElem _  _  Else                            = Else
aggregateElem af pf (FilterCell x ys)               =
  FilterCell x (aggregateElem af pf <$> ys)
aggregateElem af pf (Filter (q@(QuoteString _)) fs) =
  Filter q (aggregateElem af pf <$> fs)
aggregateElem af pf (Filter (v@(Variable path)) fs)
  | NEL.head path == pf = Filter v (aggregateElem af pf <$> fs)
  | otherwise           = Filter v (aggregateElem af pf <$> updateFs af fs)
aggregateElem af pf (Filter x fs)                   =
  Filter x (aggregateElem af pf <$> fs)
aggregateElem _  _ (Output q@(QuoteString _))       =
  Output q
aggregateElem af pf (Output v@(Variable path))
  | NEL.head path == pf = Output v
  | otherwise           = Output (Filter v [af])
aggregateElem af pf (Output f)                      =
  Output (aggregateElem af pf f)
aggregateElem af pf (TrueStatements xs)             =
  TrueStatements (aggregateElem af pf <$> xs)
aggregateElem af pf (IfLogic l r)                   =
  IfLogic (aggregateElem af pf l) (aggregateElem af pf r)
aggregateElem af pf (CaseLogic x ys)                =
  CaseLogic (aggregateElem af pf x) ((aggregateElem af pf *** aggregateElem af pf) <$> ys)
aggregateElem _  _  x                               = x

updateFs
  :: Expr
  -- ^ Aggregate sample filter to prepend
  -> [Expr]
  -- ^ List of filter cells
  -> [Expr]
updateFs af e = case e of
  []                                           -> []
  f@(FilterCell "first" _:_)                   -> f
  f@(FilterCell "firstOrDefault" _:_)          -> f
  f@(FilterCell "last" _:_)                    -> f
  f@(FilterCell "lastOrDefault" _:_)           -> f
  f@(FilterCell "countElements" _:_)           -> f
  f@(FilterCell "renderWithSeparator" _:_)     -> f
  f@(FilterCell "toSentenceWithSeparator" _:_) -> f
  (f:fs)                                       -> af:f:fs

