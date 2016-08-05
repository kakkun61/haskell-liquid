{-# LANGUAGE TemplateHaskell #-}

module Text.Liquid.VariableFinderTests where

import           Control.Monad.State.Lazy
import           Data.List.NonEmpty
import           Data.Scientific            (Scientific, fromFloatDigits)
import           Data.Text                  (Text)
import           Test.QuickCheck            hiding (output)
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck      hiding (output)
import           Test.Tasty.TH
import           Text.Liquid.Types
import           Text.Liquid.VariableFinder

-- | Test helper for scientific values
sc :: Double -> Scientific
sc d = fromFloatDigits d

case_findVariables0 = evalStateT (findVariables mzero Noop) VAny @?= []
case_findVariables1 = evalStateT (findVariables (return $ ObjectIndex "a" :| []) Noop) VAny @?= [ObjectIndex "a" :| []]
case_findVariables2 = evalStateT (findVariables mzero (RawText "")) VAny @?= []
case_findVariables3 = evalStateT (findVariables mzero (Num $ sc 1)) VAny @?= []
case_findVariables4 = evalStateT (findVariables mzero (Variable $ ObjectIndex "a" :| [])) VAny @?= [ObjectIndex "a" :| []]

case_findVariables5 = evalStateT (findVariables mzero (Equal Noop Noop)) VAny @?= []
case_findVariables6 = evalStateT (findVariables mzero (Equal (Variable $ ObjectIndex "a" :| []) Noop)) VAny @?=
  [ObjectIndex "a" :| []]

case_findVariables7 = evalStateT (findVariables mzero (Equal (Variable $ ObjectIndex "b" :| []) (Variable $ ObjectIndex "a" :| []))) VAny @?=
  [ObjectIndex "b" :| [], ObjectIndex "a" :| []]

case_findVariables8 = evalStateT (findVariables mzero
                                                (Filter (Variable $ ObjectIndex "a" :| [])
                                                        [ (Variable $ ObjectIndex "b" :| [])
                                                        , (Variable $ ObjectIndex "c" :| [])
                                                        ])) VAny @?=
  [ObjectIndex "a" :| [], ObjectIndex "b" :| [], ObjectIndex "c" :| []]

case_findVariables9 = evalStateT (findVariables mzero
                                                (CaseLogic Noop
                                                           [ ((Variable $ ObjectIndex "b" :| [])
                                                           , (Variable $ ObjectIndex "c" :| []))
                                                           ])) VAny @?=
  [ObjectIndex "b" :| [], ObjectIndex "c" :| []]

case_findVariables10 = runStateT (findVariables mzero (Gt (Variable $ ObjectIndex "a" :| []) Noop)) VStringOrNumber @?=
  [(ObjectIndex "a" :| [], VNumber)]

case_findAllVariables1 = findAllVariables [] @?= []
case_findAllVariables2 = findAllVariables [Variable $ ObjectIndex "a" :| [], Variable $ ObjectIndex "a" :| []] @?=
  [(ObjectIndex "a" :| [], VStringOrNumber)]

case_findAllVariables3 = findAllVariables [Variable $ ObjectIndex "a" :| [], Variable $ ObjectIndex "b" :| [ArrayIndex 1]] @?=
  [(ObjectIndex "a" :| [], VStringOrNumber), (ObjectIndex "b" :| [ArrayIndex 1], VStringOrNumber)]

case_findAllVariables4 = findAllVariables [Variable $ ObjectIndex "a" :| [], (Filter (Variable $ ObjectIndex "b" :| [ArrayIndex 1]) [])] @?=
  [(ObjectIndex "a" :| [], VStringOrNumber), (ObjectIndex "b" :| [ArrayIndex 1], VString)]

case_makeAggregateElem1 =
  let input = [RawText "hello ",Output (Variable (ObjectIndex "world" :| [])),RawText " yo"]
      exp   = [RawText "hello ",Output (Filter (Variable (ObjectIndex "world" :| [])) [FilterCell "first" []]),RawText " yo"]
      res   = makeAggregate (FilterCell "first" []) (ObjectIndex "user") input
  in res @?= exp

case_makeAggregateElem2 =
  let input = [RawText "hello ",Output (Filter (Variable (ObjectIndex "world" :| [])) [FilterCell "toUpper" []]),RawText " yo"]
      exp   = [RawText "hello ",Output (Filter (Variable (ObjectIndex "world" :| [])) [FilterCell "first" [], FilterCell "toUpper" []]),RawText " yo"]
      res   = makeAggregate (FilterCell "first" []) (ObjectIndex "user") input
  in res @?= exp

case_makeAggregateElem3 =
  let input = [RawText "hello ",Output (Filter (Variable (ObjectIndex "world" :| [])) [FilterCell "first" []]),RawText " yo"]
      exp   = input
      res   = makeAggregate (FilterCell "last" []) (ObjectIndex "user") input
  in res @?= exp

case_makeAggregateElem4 =
  let input = [RawText "hello ",Output (Filter (QuoteString "world") []),RawText " yo"]
      exp   = input
      res   = makeAggregate (FilterCell "first" []) (ObjectIndex "user") input
  in res @?= exp

case_makeAggregateElem5 =
  let input = [RawText "hello ",Output (Variable (ObjectIndex "user" :| [ObjectIndex "name"])),RawText " yo"]
      exp   = input
      res   = makeAggregate (FilterCell "first" []) (ObjectIndex "user") input
  in res @?= exp

case_makeAggregateElem6 =
  let input = [RawText "hello ",
               Output (Variable (ObjectIndex "user" :| [ObjectIndex "name"])),
               Output (Variable (ObjectIndex "event" :| [ObjectIndex "photo_liker"])),
               RawText " yo"
              ]
      exp   = [RawText "hello ",
               Output (Variable (ObjectIndex "user" :| [ObjectIndex "name"])),
               Output (Filter (Variable (ObjectIndex "event" :| [ObjectIndex "photo_liker"])) [FilterCell "first" []]),
               RawText " yo"
              ]
      res   = makeAggregate (FilterCell "first" []) (ObjectIndex "user") input
  in res @?= exp

case_makeAggregateElem7 =
  let input = [RawText "hello ",
               Output (Filter (Variable (ObjectIndex "user" :| [ObjectIndex "name"])) [FilterCell "countElements" []]),
               RawText " yo"
              ]
      exp   = input
      res   = makeAggregate (FilterCell "first" []) (ObjectIndex "user") input
  in res @?= exp

-- | n.b. Must remain at the bottom of the file for TH
variableFinderTests :: TestTree
variableFinderTests = $(testGroupGenerator)

