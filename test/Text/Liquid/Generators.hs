module Text.Liquid.Generators where

import           Control.Monad      (join)
import           Data.List.NonEmpty
import           Data.Monoid
import           Data.Scientific
import           Data.Text
import           Prelude            hiding (null)
import           Test.QuickCheck
import           Text.Liquid.Types


-- | Any allowed char in a variable == a-z, _-
newtype VariableChars
  = VariableChars { varChars :: Text }
  deriving (Eq, Show)

instance Arbitrary VariableChars where
  arbitrary = VariableChars . pack <$>
              (listOf1 $ elements (['a'..'z'] <>
                                   ['A'..'Z'] <>
                                   ['_','-']))

-- | Any alpha char
newtype AlphaChars
  = AlphaChars { alphaChars :: Text }
  deriving (Eq, Show)

instance Arbitrary AlphaChars where
  arbitrary = AlphaChars . pack <$>
              (listOf1 $ elements (['a'..'z'] <> ['A'..'Z']))

-- | Any allowed char type
newtype AnyChar
  = AnyChar { anyChars :: Text }
  deriving (Eq, Show)

instance Arbitrary AnyChar where
  arbitrary = AnyChar . pack <$>
              arbitrary

-- | Test helper for scientific values
sc :: Double -> Scientific
sc d = fromFloatDigits d

genJsonAddress :: Gen (JsonVarPath)
genJsonAddress = do
    h <- hd
    b <- bd
    return $ fromList (h:b)
  where hd = ObjectIndex . varChars <$> arbitrary
        bd = resize 3 $ listOf $
             oneof [ ObjectIndex . varChars <$> arbitrary
                   , ArrayIndex <$> suchThat arbitrary ((<) 0)
                   ]

genRawText :: Gen Expr
genRawText = RawText . alphaChars <$> suchThat arbitrary (not . null . alphaChars)

genNum :: Gen Expr
genNum = Num . sc <$> arbitrary

genVariable :: Gen Expr
genVariable = Variable <$> genJsonAddress

genQuoteString :: Gen Expr
genQuoteString = QuoteString . alphaChars <$> arbitrary

genCompare :: Gen Expr
genCompare =
    elements [ Equal
             , NotEqual
             , GtEqual
             , LtEqual
             , Gt
             , Lt
             ] <*> anyVal <*> anyVal
  where anyVal = oneof [ genNum
                       , genVariable
                       , genQuoteString
                       , pure Null
                       , pure Nil
                       , pure Trueth
                       , pure Falseth
                       ]

genBooleanLogic :: Gen Expr
genBooleanLogic = oneof [ cp, or, ad, cn, trth ]
  where cp = genCompare
        or = Or <$> genCompare <*> genCompare
        ad = And <$> genCompare <*> genCompare
        cn = Contains <$> genVariable <*> oneof [ genNum, genQuoteString ]
        trth = oneof [ Truthy <$> genNum
                     , Truthy <$> genQuoteString
                     , Truthy <$> genVariable
                     , pure Trueth
                     , pure Falseth
                     , pure Nil
                     , pure Null
                     ]

genIfClause :: Gen Expr
genIfClause = IfClause <$> genBooleanLogic

genIfKeyClause :: Gen Expr
genIfKeyClause = IfKeyClause <$> genVariable

genElsIfClause :: Gen Expr
genElsIfClause = ElsIfClause <$> genBooleanLogic

genElse :: Gen Expr
genElse = pure Else

genFilterCell :: Gen Expr
genFilterCell = oneof [
    pure $ FilterCell "toUpper" []
  , pure $ FilterCell "toLower" []
  , pure $ FilterCell "toTitle" []
  , FilterCell "replace" <$> sequence [ genQuoteString, genQuoteString ]
  , pure $ FilterCell "first" []
  , pure $ FilterCell "last" []
  , FilterCell "firstOrDefault" <$> oneof [ pure <$> genQuoteString, pure <$> genNum ]
  , FilterCell "lastOrDefault" <$> oneof [ pure <$> genQuoteString, pure <$> genNum ]
  , FilterCell "renderWithSeparator" <$> sequence [ genQuoteString ]
  , FilterCell "toSentenceWithSeparator" <$> sequence [ genQuoteString, genQuoteString ]
  , pure $ FilterCell "countElements" []
  ]

genFilter :: Gen Expr
genFilter = Filter                                <$>
            oneof [ genQuoteString, genVariable ] <*>
            resize 2 (listOf1 genFilterCell)

genOutput :: Gen Expr
genOutput = Output <$>
            oneof [ genFilter
                  , genVariable
                  , genQuoteString
                  ]

genTrueStatements :: Gen Expr
genTrueStatements = TrueStatements <$> arrangements
  where arrangements = oneof [
            sequence [ genRawText ]
          , sequence [ genOutput ]
          , sequence [ genRawText, genOutput ]
          , sequence [ genOutput, genRawText ]
          , sequence [ genRawText, genOutput, genRawText ]
          , sequence [ genOutput, genRawText, genOutput ]
          ]

genIfLogic :: Gen Expr
genIfLogic = oneof [ styleA
                   , styleB
                   , styleC
                   , styleD
                   ]
  where styleA = IfLogic <$> oneof[genIfClause, genIfKeyClause] <*> genTrueStatements
        styleB = IfLogic <$> (IfLogic <$> oneof[genIfClause, genIfKeyClause] <*> genTrueStatements)
                         <*> (IfLogic <$> genElse <*> genTrueStatements)
        styleC = IfLogic <$> (IfLogic <$> oneof[genIfClause, genIfKeyClause] <*> genTrueStatements)
                         <*> (IfLogic <$> genElsIfClause <*> genTrueStatements)
        styleD = IfLogic <$> (IfLogic <$> oneof[genIfClause, genIfKeyClause] <*> genTrueStatements)
                         <*> (IfLogic <$> (IfLogic <$> genElsIfClause <*> genTrueStatements)
                                      <*> (IfLogic <$> genElse <*> genTrueStatements))

genCaseLogic :: Gen Expr
genCaseLogic = CaseLogic   <$>
               genVariable <*>
               ((resize 2 $ listOf1 tup) >>=
                \l -> ((<>) l <$> oneof [ pure <$> ((,) <$> genElse <*> genTrueStatements)
                                        , pure []
                                        ]))
  where tup = (,) <$> oneof [genQuoteString, genNum] <*> genTrueStatements

genExpr :: Gen Expr
genExpr = oneof [ genRawText
                , genIfLogic
                , genOutput
                , genCaseLogic
                ]

genTemplateExpr :: Gen [Expr]
genTemplateExpr =
  concatAdj <$> listOf1 genExpr

-- | Concatenate adjacent RawText expressions in a list
--   This isn't a valid outcome from parsing and as such is illegal
concatAdj :: [Expr]
          -> [Expr]
concatAdj []                           = []
concatAdj (x:[])                       = [x]
concatAdj ((RawText x):(RawText y):[]) = [RawText $ x <> y]
concatAdj (x:y:[])                     = [x, y]
concatAdj ((RawText x):(RawText y):xs) = concatAdj ((RawText $ x <> y):xs)
concatAdj (x:y:xs)                     = x:concatAdj (y:xs)

