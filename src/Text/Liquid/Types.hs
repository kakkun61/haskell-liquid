{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Liquid.Types where

import           Control.Lens.TH    (makePrisms)
import           Data.Aeson.Types   (Value)
import           Data.Hashable      (Hashable)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Scientific    (Scientific)
import           Data.Text          (Text)
import           Data.Validation    (AccValidation)
import           GHC.Generics       (Generic)


--------------------------------------------------------------------------------
-- * Liquid Template Data
--------------------------------------------------------------------------------

type JsonVarPath = NonEmpty VarIndex

data VarIndex
  = ObjectIndex Text
  | ArrayIndex Int
  deriving (Eq, Ord, Show, Generic)

instance Hashable VarIndex

data Expr
  = Noop
  | RawText Text
  | Num Scientific
  | Variable JsonVarPath
  | QuoteString Text
  | Equal Expr Expr
  | NotEqual Expr Expr
  | GtEqual Expr Expr
  | LtEqual Expr Expr
  | Gt Expr Expr
  | Lt Expr Expr
  | Or Expr Expr
  | And Expr Expr
  | Contains Expr Expr
  | Nil
  | Null
  | Trueth
  | Falseth
  | Truthy Expr
  | IfClause Expr
  | IfKeyClause Expr
  | ElsIfClause Expr
  | Else
  | AssignClause Expr Expr
  | FilterCell Text [Expr]
  | Filter Expr [Expr]
  | Output Expr
  | TrueStatements [Expr]
  | IfLogic Expr Expr
  | CaseLogic Expr [(Expr, Expr)]
  deriving (Eq, Show)
makePrisms ''Expr

--------------------------------------------------------------------------------
-- * Error types
--------------------------------------------------------------------------------

type Rendering a = AccValidation [LiquidError] a

data LiquidError
  = TemplateParsingError Text Text [Text]
  | JsonValueError Text
  | JsonValueNotFound JsonVarPath
  | NotAStringOrNumberJsonValue (Rendering Value)
  | ImpossibleComparison Text Text
  | ImpossibleTruthEvaluation Expr
  | RenderingFailure Text
  | LiquidError Text
  deriving (Eq, Show)

