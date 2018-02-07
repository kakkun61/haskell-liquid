{-# LANGUAGE TemplateHaskell #-}

module Text.Liquid.RendererTests where

import           Control.Lens             hiding ((.=))
import           Data.Aeson               hiding (Null)
import qualified Data.Aeson               as A
import           Data.Aeson.Lens
import           Data.List.NonEmpty
import           Data.Scientific
import           Data.Text                (Text, empty, pack, replace, toLower,
                                           toTitle, toUpper)
import           Data.Validation
import qualified Data.Vector              as V
import           Test.QuickCheck          hiding (output)
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck    hiding (output)
import           Test.Tasty.TH
import           Text.Liquid.Helpers
import           Text.Liquid.Renderer
import           Text.Liquid.Types

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

-- | Test helper for scientific values
sc :: Double -> Scientific
sc d = fromFloatDigits d

--------------------------------------------------------------------------------
-- * applyCellsM
--------------------------------------------------------------------------------

case_applyCellsM1 = applyCellsM (toJSON ("abc" :: Text))
                                [FilterCell "toUpper" []] @?=
  Just "ABC"

case_applyCellsM2 = applyCellsM (toJSON (["hello", "world"] :: [Text]))
                                [FilterCell "toUpper" []] @?=
  Nothing

case_applyCellsM3 = applyCellsM (toJSON (["hello", "world"] :: [Text]))
                                [FilterCell "last" []] @?=
  Just "world"

case_applyCellsM4 = applyCellsM (toJSON ([] :: [Text]))
                                [ FilterCell "firstOrDefault" [QuoteString "abc"]
                                , FilterCell "toUpper" []] @?=
  Just "ABC"

--------------------------------------------------------------------------------
-- * applyArrayFilterM
--------------------------------------------------------------------------------

case_applyArrayFilterM1 = applyArrayFilterM []
                                            (FilterCell "first" []) @?=
  Just ""

case_applyArrayFilterM2 = applyArrayFilterM [toJSON ("hello" :: Text)]
                                            (FilterCell "first" []) @?=
  Just "hello"

case_applyArrayFilterM3 = applyArrayFilterM [toJSON ("hello" :: Text), toJSON ("world" :: Text)]
                                            (FilterCell "first" []) @?=
  Just "hello"

case_applyArrayFilterM4 = applyArrayFilterM []
                                            (FilterCell "firstOrDefault" [QuoteString "foo"]) @?=
  Just "foo"

case_applyArrayFilterM5 = applyArrayFilterM [toJSON ("hello" :: Text)]
                                            (FilterCell "firstOrDefault" [QuoteString "foo"]) @?=
  Just "hello"

case_applyArrayFilterM6 = applyArrayFilterM []
                                            (FilterCell "last" []) @?=
  Just ""

case_applyArrayFilterM7 = applyArrayFilterM [toJSON ("hello" :: Text)]
                                            (FilterCell "last" []) @?=
  Just "hello"

case_applyArrayFilterM8 = applyArrayFilterM [toJSON ("hello" :: Text), toJSON ("world" :: Text)]
                                            (FilterCell "last" []) @?=
  Just "world"

case_applyArrayFilterM9 = applyArrayFilterM []
                                            (FilterCell "lastOrDefault" [Num 123]) @?=
  Just "123"

case_applyArrayFilterM10 = applyArrayFilterM [toJSON ("hello" :: Text)]
                                             (FilterCell "lastOrDefault" [QuoteString "abc"]) @?=
  Just "hello"

case_applyArrayFilterM11 = applyArrayFilterM [toJSON ("hello" :: Text)]
                                             (FilterCell "toUpper" [QuoteString "abc"]) @?=
  Nothing

case_applyArrayFilterM12 = applyArrayFilterM []
                                             (FilterCell "countElements" []) @?=
  Just "0"

case_applyArrayFilterM13 = applyArrayFilterM [toJSON ("foo" :: Text), toJSON ("bar" :: Text)]
                                             (FilterCell "countElements" []) @?=
  Just "2"

case_applyArrayFilterM14 = applyArrayFilterM []
                                             (FilterCell "renderWithSeparator" [QuoteString ", "]) @?=
  Just ""

case_applyArrayFilterM15 = applyArrayFilterM [toJSON ("foo" :: Text)]
                                             (FilterCell "renderWithSeparator" [QuoteString ", "]) @?=
  Just "foo"

case_applyArrayFilterM16 = applyArrayFilterM [toJSON ("foo" :: Text), toJSON ("bar" :: Text)]
                                             (FilterCell "renderWithSeparator" [QuoteString ", "]) @?=
  Just "foo, bar"

case_applyArrayFilterM17 = applyArrayFilterM [toJSON ("foo" :: Text), toJSON ("bar" :: Text), toJSON ("baz" :: Text)]
                                             (FilterCell "renderWithSeparator" [QuoteString ", "]) @?=
  Just "foo, bar, baz"

case_applyArrayFilterM18 = applyArrayFilterM []
                                             (FilterCell "toSentenceWithSeparator" [QuoteString ", ", QuoteString " and "]) @?=
  Just ""

case_applyArrayFilterM19 = applyArrayFilterM [toJSON ("foo" :: Text)]
                                             (FilterCell "toSentenceWithSeparator" [QuoteString ", ", QuoteString " and "]) @?=
  Just "foo"

case_applyArrayFilterM20 = applyArrayFilterM [toJSON ("foo" :: Text), toJSON ("bar" :: Text)]
                                             (FilterCell "toSentenceWithSeparator" [QuoteString ", ", QuoteString " and "]) @?=
  Just "foo and bar"

case_applyArrayFilterM21 = applyArrayFilterM [toJSON ("foo" :: Text), toJSON ("bar" :: Text), toJSON ("baz" :: Text)]
                                             (FilterCell "toSentenceWithSeparator" [QuoteString ", ", QuoteString " and "]) @?=
  Just "foo, bar and baz"

--------------------------------------------------------------------------------
-- * arrayFilterM
--------------------------------------------------------------------------------

case_arrayFilterM1 = arrayFilterM (toJSON ("abc" :: Text))
                                  (FilterCell "toUpper" []) @?=
  Just "ABC"

case_arrayFilterM2 = arrayFilterM (object [])
                                  (FilterCell "toUpper" []) @?=
  Nothing

case_arrayFilterM3 = arrayFilterM (toJSON ([] :: [Text]))
                                  (FilterCell "first" []) @?=
  Just ""

--------------------------------------------------------------------------------
-- * applyFilterM
--------------------------------------------------------------------------------

prop_applyFilterM1 = \t ->
  ((applyFilterM t (FilterCell "toUpper" [])) == (Just $ toUpper t))

prop_applyFilterM2 = \t ->
  ((applyFilterM t (FilterCell "toLower" [])) == (Just $ toLower t))

prop_applyFilterM3 = \t ->
  ((applyFilterM t (FilterCell "toTitle" [])) == (Just $ toTitle t))

prop_applyFilterM4 = \t ->
  ((applyFilterM t (FilterCell "replace" [QuoteString "a", QuoteString "b"])) == (Just $ replace "a" "b" t))

prop_applyFilterM5 = \t ->
  ((applyFilterM t (FilterCell "toLower" [QuoteString "a"])) == Nothing)

case_applyFilterM1 = applyFilterM empty (FilterCell "toLower" []) @?= Just empty
case_applyFilterM2 = applyFilterM empty (FilterCell "doesn\'t_exist_fn" []) @?= Nothing
case_applyFilterM3 = applyFilterM empty Trueth @?= Nothing

--------------------------------------------------------------------------------
-- * applyFilter
--------------------------------------------------------------------------------

case_applyFilter1 = applyFilter (object [])
                                (Filter (QuoteString "abc")
                                [FilterCell "toUpper" []]) @?=
  AccSuccess "ABC"

case_applyFilter2 = applyFilter (object [])
                                (Filter (QuoteString "abc")
                                [ FilterCell "toUpper" []
                                , FilterCell "toLower" []
                                ]) @?=
  AccSuccess "abc"

case_applyFilter3 = applyFilter (object [])
                                (Filter (QuoteString "abc")
                                []) @?=
  AccSuccess "abc"

case_applyFilter4 = applyFilter (object ["a" .= ("abc" :: Text)])
                                (Filter (Variable $ ObjectIndex "a" :| [])
                                [FilterCell "toUpper" []]) @?=
  AccSuccess "ABC"

case_applyFilter5 = applyFilter (object [])
                                (Filter (Variable $ ObjectIndex "a" :| [])
                                [FilterCell "toUpper" []]) @?=
  AccFailure [ RenderingFailure "Variable filtration fn failure"
             , JsonValueNotFound $ ObjectIndex "a" :| []
             ]

case_applyFilter6 = applyFilter (object ["a" .= (["abc", "def"] :: [Text])])
                                (Filter (Variable $ ObjectIndex "a" :| [])
                                [FilterCell "toUpper" []]) @?=
  AccFailure [ RenderingFailure "Variable filtration fn failure" ]

case_applyFilter7 = applyFilter (object ["a" .= (["abc", "def"] :: [Text])])
                                (Filter (Variable $ ObjectIndex "a" :| [])
                                [FilterCell "first" []]) @?=
  AccSuccess "abc"

case_applyFilter8 = applyFilter (object ["a" .= (["abc", "def"] :: [Text])])
                                (Filter (Variable $ ObjectIndex "a" :| [])
                                [ FilterCell "last" []
                                , FilterCell "toUpper" []]) @?=
  AccSuccess "DEF"

case_applyFilter9 = applyFilter (object ["a" .= ([456, 123] :: [Int])])
                                (Filter (Variable $ ObjectIndex "a" :| [])
                                [ FilterCell "last" []
                                , FilterCell "toUpper" []]) @?=
  AccSuccess "123"

case_applyFilter10 = applyFilter (object []) Trueth @?=
  AccFailure [ LiquidError "Filter Bug!" ]

--------------------------------------------------------------------------------
-- * extractValue
--------------------------------------------------------------------------------

case_extractValue1 = extractValue (object ["a" .= ("abc" :: Text)])
                                  (ObjectIndex "a" :| []) @?= AccSuccess "abc"

case_extractValue2 = let path = ObjectIndex "a" :| []
                         res  = extractValue (object []) path
                         exp  = AccFailure [ JsonValueNotFound path]
                     in res @?= exp

case_extractValue4 = extractValue (object ["a" .= (["b", "c"] :: [Text])])
                                  (ObjectIndex "a" :| [ArrayIndex 1]) @?= AccSuccess "c"

case_extractValue5 = extractValue (object ["a" .= object ["b" .= (["b", "c"] :: [Text])]])
                                  (ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 1])  @?= AccSuccess "c"

case_extractValue6 = extractValue (object ["event" .= object ["b" .= (["b", "c"] :: [Text])]])
                                  (ObjectIndex "event" :| [ObjectIndex "b", ArrayIndex 1])  @?= AccSuccess "c"

case_extractValue7 = extractValue (object ["event" .= object ["b" .= (["b", "c"] :: [Text])]])
                                  (ObjectIndex "b" :| [ArrayIndex 1])  @?= AccSuccess "c"

case_extractValue8 = extractValue (object ["user" .= (object ["a" .= object ["b" .= (["b", "c"] :: [Text])]])])
                                  (ObjectIndex "user" :| [ObjectIndex "a", ObjectIndex "b", ArrayIndex 0])  @?= AccSuccess "b"

case_extractValue9 = extractValue (object ["user" .= (object ["a" .= object ["b" .= (["b", "c"] :: [Text])]])])
                                  (ObjectIndex "a" :| [ ObjectIndex "b", ArrayIndex 0])  @?=
                                    AccFailure [ JsonValueNotFound $ ObjectIndex "a" :| [ ObjectIndex "b", ArrayIndex 0]]

--------------------------------------------------------------------------------
-- * bothSidesEqual
--------------------------------------------------------------------------------

case_bothSidesEqual1 = let l   = Trueth
                           r   = Trueth
                           j   = object []
                           exp = AccSuccess True
                           res = bothSidesEqual j l r
                       in res @?= exp

case_bothSidesEqual2 = let l   = QuoteString "a"
                           r   = QuoteString "a"
                           j   = object []
                           exp = AccSuccess True
                           res = bothSidesEqual j l r
                       in res @?= exp

case_bothSidesEqual3 = let l   = QuoteString "a"
                           r   = QuoteString "b"
                           j   = object []
                           exp = AccSuccess False
                           res = bothSidesEqual j l r
                       in res @?= exp

case_bothSidesEqual4 = let l   = Variable $ ObjectIndex "a" :| []
                           r   = QuoteString "b"
                           j   = object ["a" .= ("b" :: Text)]
                           exp = AccSuccess True
                           res = bothSidesEqual j l r
                       in res @?= exp

case_bothSidesEqual5 = let l   = Variable $ ObjectIndex "a" :| []
                           r   = QuoteString "b"
                           j   = object []
                           exp = AccFailure [ JsonValueNotFound $ ObjectIndex "a" :| [] ]
                           res = bothSidesEqual j l r
                       in res @?= exp

case_bothSidesEqual6 = let l   = QuoteString "b"
                           r   = Variable $ ObjectIndex "a" :| []
                           j   = object ["a" .= ("b" :: Text)]
                           exp = AccSuccess True
                           res = bothSidesEqual j l r
                       in res @?= exp

case_bothSidesEqual7 = let l   = QuoteString "b"
                           r   = Variable $ ObjectIndex "a" :| []
                           j   = object []
                           exp = AccFailure [ JsonValueNotFound $ ObjectIndex "a" :| [] ]
                           res = bothSidesEqual j l r
                       in res @?= exp

case_bothSidesEqual8 = let l   = Variable $ ObjectIndex "a" :| []
                           r   = Variable $ ObjectIndex "b" :| []
                           j   = object ["a" .= ("b" :: Text), "b" .= ("b" :: Text)]
                           exp = AccSuccess True
                           res = bothSidesEqual j l r
                       in res @?= exp

case_bothSidesEqual9 = let l   = Variable $ ObjectIndex "a" :| []
                           r   = Variable $ ObjectIndex "b" :| []
                           j   = object ["a" .= ("b" :: Text), "b" .= ("a" :: Text)]
                           exp = AccSuccess False
                           res = bothSidesEqual j l r
                       in res @?= exp

case_bothSidesEqual10 = let l   = Variable $ ObjectIndex "a" :| []
                            r   = Variable $ ObjectIndex "b" :| []
                            j   = object ["a" .= ("b" :: Text)]
                            exp = AccFailure [ JsonValueNotFound $ ObjectIndex "b" :| [] ]
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual11 = let l   = Num $ sc 1
                            r   = Num $ sc 1
                            j   = object []
                            exp = AccSuccess True
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual12 = let l   = Num $ sc 1
                            r   = Num $ sc 2
                            j   = object []
                            exp = AccSuccess False
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual13 = let l   = Num $ sc 1
                            r   = Variable $ ObjectIndex "a" :| []
                            j   = object ["a" .= (1 :: Integer)]
                            exp = AccSuccess True
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual14 = let l   = Variable $ ObjectIndex "a" :| []
                            r   = Num $ sc 1
                            j   = object ["a" .= (1 :: Integer)]
                            exp = AccSuccess True
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual15 = let l   = Variable $ ObjectIndex "a" :| []
                            r   = Variable $ ObjectIndex "b" :| []
                            j   = object ["a" .= (1 :: Integer), "b" .= (1 :: Integer)]
                            exp = AccSuccess True
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual16 = let l   = Variable $ ObjectIndex "a" :| []
                            r   = Variable $ ObjectIndex "b" :| []
                            j   = object ["a" .= (1 :: Integer), "b" .= (2 :: Integer)]
                            exp = AccSuccess False
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual17 = let l   = Variable $ ObjectIndex "a" :| []
                            r   = Trueth
                            j   = object []
                            exp = AccFailure [JsonValueNotFound (ObjectIndex "a" :| [])]
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual18 = let l   = Variable $ ObjectIndex "a" :| []
                            r   = Trueth
                            j   = object ["a" .= True]
                            exp = AccSuccess True
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual19 = let l   = Trueth
                            r   = Variable $ ObjectIndex "a" :| []
                            j   = object ["a" .= True]
                            exp = AccSuccess True
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual20 = let l   = Falseth
                            r   = Variable $ ObjectIndex "a" :| []
                            j   = object ["a" .= True]
                            exp = AccSuccess False
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual21 = let l   = Variable $ ObjectIndex "a" :| []
                            r   = Falseth
                            j   = object ["a" .= False]
                            exp = AccSuccess True
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual22 = let l   = Trueth
                            r   = Variable $ ObjectIndex "a" :| []
                            j   = object ["a" .= False]
                            exp = AccSuccess False
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual23 = let l   = Falseth
                            r   = Variable $ ObjectIndex "a" :| []
                            j   = object ["a" .= True]
                            exp = AccSuccess False
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual24 = let l   = Null
                            r   = Variable $ ObjectIndex "a" :| []
                            j   = object ["a" .= A.Null]
                            exp = AccSuccess True
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual25 = let l   = Variable $ ObjectIndex "a" :| []
                            r   = Null
                            j   = object ["a" .= A.Null]
                            exp = AccSuccess True
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual26 = let l   = Nil
                            r   = Variable $ ObjectIndex "a" :| []
                            j   = object ["a" .= A.Null]
                            exp = AccSuccess True
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual27 = let l   = Variable $ ObjectIndex "a" :| []
                            r   = Nil
                            j   = object ["a" .= A.Null]
                            exp = AccSuccess True
                            res = bothSidesEqual j l r
                        in res @?= exp

case_bothSidesEqual28 = let l   = Variable $ ObjectIndex "a" :| []
                            r   = Nil
                            j   = object ["a" .= (1 :: Integer)]
                            exp = AccSuccess False
                            res = bothSidesEqual j l r
                        in res @?= exp


--------------------------------------------------------------------------------
-- * varComparisons
--------------------------------------------------------------------------------

case_varComparisons1 = let l   = Num $ sc 1
                           r   = Variable $ ObjectIndex "a" :| []
                           j   = object ["a" .= (2 :: Integer)]
                           exp = AccSuccess True
                           res = varComparisons j (<) l r
                       in res @?= exp

case_varComparisons2 = let l   = Num $ sc 1
                           r   = Variable $ ObjectIndex "a" :| []
                           j   = object ["a" .= (2 :: Integer)]
                           exp = AccSuccess False
                           res = varComparisons j (>) l r
                       in res @?= exp

case_varComparisons3 = let l   = Num $ sc 1
                           r   = Variable $ ObjectIndex "a" :| []
                           j   = object []
                           exp = AccFailure [ JsonValueNotFound $ ObjectIndex "a" :| [] ]
                           res = varComparisons j (>) l r
                       in res @?= exp

case_varComparisons4 = let l   = Variable $ ObjectIndex "a" :| []
                           r   = Variable $ ObjectIndex "b" :| []
                           j   = object ["a" .= (1 :: Integer), "b" .= (2 :: Integer)]
                           exp = AccSuccess False
                           res = varComparisons j (>=) l r
                       in res @?= exp

case_varComparisons5 = let l   = Variable $ ObjectIndex "a" :| []
                           r   = Variable $ ObjectIndex "b" :| []
                           j   = object ["a" .= (1 :: Integer), "b" .= (2 :: Integer)]
                           exp = AccSuccess True
                           res = varComparisons j (<=) l r
                       in res @?= exp

case_varComparisons6 = let l   = Variable $ ObjectIndex "a" :| []
                           r   = Variable $ ObjectIndex "b" :| []
                           j   = object ["a" .= (1 :: Integer), "b" .= ("foo" :: Text)]
                           exp = AccFailure [ ImpossibleComparison "a" "Number not found at variableb" ]
                           res = varComparisons j (>) l r
                       in res @?= exp

case_varComparisons7 = let l   = Variable $ ObjectIndex "a" :| []
                           r   = Variable $ ObjectIndex "b" :| []
                           j   = object ["a" .= (1 :: Integer)]
                           exp = AccFailure [ ImpossibleComparison "Number not found at variablea"
                                                                   "Number not found at variableb" ]
                           res = varComparisons j (>) l r
                       in res @?= exp

case_varComparisons8 = let l   = Variable $ ObjectIndex "a" :| []
                           r   = Trueth
                           j   = object ["a" .= (1 :: Integer), "b" .= ("foo" :: Text)]
                           exp = AccFailure [ ImpossibleComparison "a" "true" ]
                           res = varComparisons j (<=) l r
                       in res @?= exp

--------------------------------------------------------------------------------
-- * evalTruthiness
--------------------------------------------------------------------------------

case_evalTruthiness_3 = let j   = object []
                            e   = Null
                            exp = AccSuccess False
                            res = evalTruthiness j e
                        in res @?= exp

case_evalTruthiness_2 = let j   = object []
                            e   = Nil
                            exp = AccSuccess False
                            res = evalTruthiness j e
                        in res @?= exp

case_evalTruthiness_1 = let j   = object []
                            e   = Truthy . Num $ sc 123
                            exp = AccSuccess True
                            res = evalTruthiness j e
                        in res @?= exp

case_evalTruthiness0 = let j   = object []
                           e   = Truthy $ QuoteString "a"
                           exp = AccSuccess True
                           res = evalTruthiness j e
                       in res @?= exp

case_evalTruthiness1 = let j   = object ["a" .= True]
                           e   = Truthy $ Variable $ ObjectIndex "a" :| []
                           exp = AccSuccess True
                           res = evalTruthiness j e
                       in res @?= exp

case_evalTruthiness2 = let j   = object ["a" .= False]
                           e   = Truthy $ Variable $ ObjectIndex "a" :| []
                           exp = AccSuccess False
                           res = evalTruthiness j e
                       in res @?= exp

case_evalTruthiness3 = let j   = object []
                           e   = Truthy $ Variable $ ObjectIndex "a" :| []
                           exp = AccFailure [ JsonValueNotFound $ ObjectIndex "a" :| [] ]
                           res = evalTruthiness j e
                       in res @?= exp

case_evalTruthiness4 = let j   = object ["a" .= ("foo" :: Text)]
                           e   = Or (Equal (Variable $ ObjectIndex "a" :| []) (QuoteString "foo")) Falseth
                           exp = AccSuccess True
                           res = evalTruthiness j e
                       in res @?= exp

case_evalTruthiness5 = let j   = object ["a" .= ("foo" :: Text)]
                           e   = And (Equal (Variable $ ObjectIndex "a" :| []) (QuoteString "foo")) Falseth
                           exp = AccSuccess False
                           res = evalTruthiness j e
                       in res @?= exp

case_evalTruthiness6 = let j   = object []
                           e   = Truthy $ Variable $ ObjectIndex "a" :| []
                           exp = AccFailure [ JsonValueNotFound $ ObjectIndex "a" :| []]
                           res = evalTruthiness j e
                       in res @?= exp

case_evalTruthiness7 = let j   = object []
                           e   = Else
                           exp = AccFailure [ ImpossibleTruthEvaluation Else ]
                           res = evalTruthiness j e
                       in res @?= exp

case_evalTruthiness8 = let j   = object ["a" .= ("b" :: Text)]
                           e   = Contains (Variable $ ObjectIndex "a" :| []) (Num $ sc 1)
                           exp = AccSuccess False
                           res = evalTruthiness j e
                       in res @?= exp

--------------------------------------------------------------------------------
-- * evalKeyTruthiness
--------------------------------------------------------------------------------

case_evalKeyTruthiness1 = let j   = object []
                              e   = Trueth
                              exp = AccFailure [ RenderingFailure "Can't evalulate if key on anything other than json context variables" ]
                              res = evalKeyTruthiness j e
                          in res @?= exp

case_evalKeyTruthiness2 = let j   = object []
                              e   = Variable $ ObjectIndex "a" :| []
                              exp = AccSuccess False
                              res = evalKeyTruthiness j e
                          in res @?= exp

case_evalKeyTruthiness3 = let j   = object ["a" .= A.Null]
                              e   = Variable $ ObjectIndex "a" :| []
                              exp = AccSuccess False
                              res = evalKeyTruthiness j e
                          in res @?= exp

case_evalKeyTruthiness4 = let j   = object ["a" .= (1 :: Integer)]
                              e   = Variable $ ObjectIndex "a" :| []
                              exp = AccSuccess True
                              res = evalKeyTruthiness j e
                          in res @?= exp

case_evalKeyTruthiness5 = let j   = object ["a" .= ("hello" :: Text)]
                              e   = Variable $ ObjectIndex "a" :| []
                              exp = AccSuccess True
                              res = evalKeyTruthiness j e
                          in res @?= exp

case_evalKeyTruthiness6 = let j   = object ["event" .= object [ "a" .= ("hello" :: Text) ]]
                              e   = Variable $ ObjectIndex "a" :| []
                              exp = AccSuccess True
                              res = evalKeyTruthiness j e
                          in res @?= exp

case_evalKeyTruthiness7 = let j   = object ["user" .= object [ "a" .= ("hello" :: Text) ]]
                              e   = Variable $ ObjectIndex "user" :| [ObjectIndex "a"]
                              exp = AccSuccess True
                              res = evalKeyTruthiness j e
                          in res @?= exp

case_evalKeyTruthiness8 = let j   = object ["event" .= object [ "a" .= ("hello" :: Text) ]]
                              e   = Variable $ ObjectIndex "event" :| [ObjectIndex "a"]
                              exp = AccSuccess True
                              res = evalKeyTruthiness j e
                          in res @?= exp

case_evalKeyTruthiness9 = let j   = object ["event" .= object [ "a" .= ("hello" :: Text) ]]
                              e   = Variable $ ObjectIndex "b" :| []
                              exp = AccSuccess False
                              res = evalKeyTruthiness j e
                          in res @?= exp

--------------------------------------------------------------------------------
-- * getStringsFromArray
--------------------------------------------------------------------------------

case_getStringsFromArray1 = (getStringsFromArray <$> (preview _Value ("[]" :: Text))) @?= Just []
case_getStringsFromArray2 = (getStringsFromArray <$> (preview _Value ("[\"a\"]" :: Text))) @?= Just ["a"]
case_getStringsFromArray3 = (getStringsFromArray <$> (preview _Value ("[\"a\", \"foo\"]" :: Text))) @?= Just ["a", "foo"]
case_getStringsFromArray4 = (getStringsFromArray <$> (preview _Value ("[\"a\", 123, \"b\", 1]" :: Text))) @?= Just ["a", "b"]

--------------------------------------------------------------------------------
-- * getNumbersFromArray
--------------------------------------------------------------------------------

case_getNumbersFromArray1 = (getNumbersFromArray <$> (preview _Value ("[]" :: Text))) @?= Just []
case_getNumbersFromArray2 = (getNumbersFromArray <$> (preview _Value ("[\"a\"]" :: Text))) @?= Just []
case_getNumbersFromArray3 = (getNumbersFromArray <$> (preview _Value ("[1]" :: Text))) @?= Just [sc $ 1]
case_getNumbersFromArray4 = (getNumbersFromArray <$> (preview _Value ("[\"a\", 123, \"b\", 1]" :: Text))) @?= Just [sc 123, sc 1]

--------------------------------------------------------------------------------
-- * containsCheck
--------------------------------------------------------------------------------

case_containsCheck1 = let j = object []
                          l = Variable $ ObjectIndex "a" :| []
                          r = QuoteString "b"
                          exp = AccFailure [ JsonValueNotFound $ ObjectIndex "a" :| []]
                          res = containsCheck j l r
                      in res @?= exp

case_containsCheck2 = let a = maybe (V.empty) id (preview _Array ("[]" :: Text))
                          j = object ["a" .= a]
                          l = Variable $ ObjectIndex "a" :| []
                          r = QuoteString "b"
                          exp = AccSuccess False
                          res = containsCheck j l r
                      in res @?= exp

case_containsCheck3 = let a = maybe (V.empty) id (preview _Array ("[\"b\"]" :: Text))
                          j = object ["a" .= a]
                          l = Variable $ ObjectIndex "a" :| []
                          r = QuoteString "b"
                          exp = AccSuccess True
                          res = containsCheck j l r
                      in res @?= exp

case_containsCheck4 = let a = maybe (V.empty) id (preview _Array ("[123, \"b\", \"foo\"]" :: Text))
                          j = object ["a" .= a]
                          l = Variable $ ObjectIndex "a" :| []
                          r = QuoteString "b"
                          exp = AccSuccess True
                          res = containsCheck j l r
                      in res @?= exp

case_containsCheck5 = let a = maybe (V.empty) id (preview _Array ("[123, \"b\", \"foo\"]" :: Text))
                          j = object ["a" .= a]
                          l = Variable $ ObjectIndex "a" :| []
                          r = Num $ sc 123
                          exp = AccSuccess True
                          res = containsCheck j l r
                      in res @?= exp

case_containsCheck6 = let a = maybe (V.empty) id (preview _Array ("[123, \"b\", \"foo\"]" :: Text))
                          j = object ["a" .= a]
                          l = Variable $ ObjectIndex "a" :| []
                          r = Num $ sc 124
                          exp = AccSuccess False
                          res = containsCheck j l r
                      in res @?= exp

case_containsCheck7 = let a = maybe (V.empty) id (preview _Array ("hello" :: Text))
                          j = object ["b" .= a]
                          l = Variable $ ObjectIndex "a" :| []
                          r = Num $ sc 124
                          exp = AccFailure [ JsonValueNotFound $ ObjectIndex "a" :| []]
                          res = containsCheck j l r
                      in res @?= exp

case_containsCheck8 = let j = object []
                          l = Variable $ ObjectIndex "a" :| []
                          r = Else
                          exp = AccFailure [ ImpossibleComparison "Contains" "{% else %}" ]
                          res = containsCheck j l r
                      in res @?= exp

case_containsCheck9 = let j = object []
                          l = Else
                          r = Variable $ ObjectIndex "a" :| []
                          exp = AccFailure [ LiquidError "Contains checks can only be performed on arrays (i.e. Variables)" ]
                          res = containsCheck j l r
                      in res @?= exp

--------------------------------------------------------------------------------
-- * numberOrTextFormat
--------------------------------------------------------------------------------

case_numberOrTextFormat1 = numberOrTextFormat (pure . toJSON $ sc 123) @?= AccSuccess "123"
case_numberOrTextFormat2 = numberOrTextFormat (pure . toJSON $ sc 123.1) @?= AccSuccess "123.1"
case_numberOrTextFormat3 = numberOrTextFormat (pure . toJSON $ ("hello" :: Text)) @?= AccSuccess "hello"
case_numberOrTextFormat4 = numberOrTextFormat (pure . toJSON $ True) @?=
  AccFailure [ NotAStringOrNumberJsonValue (AccSuccess . toJSON $ True) ]

--------------------------------------------------------------------------------
-- * renderText
--------------------------------------------------------------------------------

case_renderText1 = renderText (object []) Noop @?= AccSuccess ""
case_renderText2 = renderText (object []) (RawText "abc") @?= AccSuccess "abc"
case_renderText3 = renderText (object []) (Num $ sc 123) @?= AccSuccess "123"
case_renderText4 = renderText (object ["a" .= ("foo" :: Text)]) (Variable $ ObjectIndex "a" :| []) @?= AccSuccess "foo"
case_renderText5 = renderText (object ["a" .= (123 :: Integer)]) (Variable $ ObjectIndex "a" :| []) @?= AccSuccess "123"
case_renderText6 = renderText (object []) (QuoteString "foo") @?= AccSuccess "foo"
case_renderText7 = renderText (object []) Else @?= AccFailure [ RenderingFailure "Can't render this type: {% else %}"]

--------------------------------------------------------------------------------
-- * evalCaseLogic
--------------------------------------------------------------------------------

case_evalCaseLogic1 = evalCaseLogic (object []) (pure $ toJSON ("a" :: Text)) [] @?= AccSuccess ("", object [])
case_evalCaseLogic2 = evalCaseLogic (object [])
                                    (pure $ toJSON . sc $ 123)
                                    [(Num $ sc 123, TrueStatements [RawText "foo", RawText "bar"])] @?= AccSuccess ("foobar", object [])

case_evalCaseLogic3 = evalCaseLogic (object [])
                                    (pure $ toJSON $ sc 123)
                                    [(Num $ sc 124, TrueStatements [RawText "foo", RawText "bar"])] @?= AccSuccess ("", object [])

case_evalCaseLogic4 = evalCaseLogic (object [])
                                    (pure $ toJSON $ sc 123)
                                    [ (Num $ sc 124, TrueStatements [RawText "foo", RawText "bar"])
                                    , (Num $ sc 123, TrueStatements [RawText "foo", RawText "baz"])
                                    ] @?= AccSuccess ("foobaz", object [])

case_evalCaseLogic5 = evalCaseLogic (object [])
                                    (pure $ toJSON ("hello" :: Text))
                                    [(QuoteString "hello", TrueStatements [RawText "foo", RawText "bar"])] @?= AccSuccess ("foobar", object [])

case_evalCaseLogic6 = evalCaseLogic (object [])
                                    (pure $ toJSON ("helo" :: Text))
                                    [(QuoteString "hello", TrueStatements [RawText "foo", RawText "bar"])] @?= AccSuccess ("", object [])

case_evalCaseLogic7 = evalCaseLogic (object [])
                                    (pure $ toJSON ("helo" :: Text))
                                    [ (QuoteString "hello", TrueStatements [RawText "foo", RawText "bar"])
                                    , (QuoteString "helo", TrueStatements [RawText "foo", RawText "baz"])
                                    ] @?= AccSuccess ("foobaz", object [])

case_evalCaseLogic8 = evalCaseLogic (object [])
                                    (pure $ toJSON ("helo" :: Text))
                                    [ (Else, TrueStatements [RawText "foo", RawText "bar"])
                                    ] @?= AccSuccess ("foobar", object [])

case_evalCaseLogic9 = evalCaseLogic (object [])
                                    (pure $ toJSON ("helo" :: Text))
                                    [ (QuoteString "hello", TrueStatements [RawText "foo", RawText "baz"])
                                    , (Else, TrueStatements [RawText "foo", RawText "bar"])
                                    ] @?= AccSuccess ("foobar", object [])

case_evalCaseLogic10 = evalCaseLogic (object [])
                                     (pure $ toJSON ("helo" :: Text))
                                     [(Trueth, TrueStatements [RawText "foo", RawText "bar"])] @?=
                                      AccFailure [ RenderingFailure "Impossible case pattern evaluation" ]

case_evalCaseLogic11 = evalCaseLogic (object ["a" .= ("quux" :: Text)])
                                    (pure $ toJSON ("helo" :: Text))
                                    [ (QuoteString "hello", TrueStatements [RawText "foo", RawText "baz"])
                                    , (Else, TrueStatements [RawText "foo", RawText "bar", Output $ Variable $ ObjectIndex "a" :| []])
                                    ] @?= AccSuccess ("foobarquux", object ["a" .= ("quux" :: Text)])

--------------------------------------------------------------------------------
-- * evalLogic
--------------------------------------------------------------------------------

case_evalLogic1 = let j = object []
                      p = pure True
                      t = []
                      res = evalLogic j p t
                      exp = AccSuccess ("", j)
                  in res @?= exp

case_evalLogic2 = let j = object []
                      p = pure False
                      t = []
                      res = evalLogic j p t
                      exp = AccSuccess ("", j)
                  in res @?= exp

case_evalLogic3 = let j = object []
                      p = pure True
                      t = [RawText "foo"]
                      res = evalLogic j p t
                      exp = AccSuccess ("foo", j)
                  in res @?= exp

case_evalLogic4 = let j = object []
                      p = pure True
                      t = [RawText "foo", RawText "bar"]
                      res = evalLogic j p t
                      exp = AccSuccess ("foobar", j)
                  in res @?= exp

case_evalLogic5 = let j = object []
                      p = pure False
                      t = [RawText "foo", RawText "bar"]
                      res = evalLogic j p t
                      exp = AccSuccess ("", j)
                  in res @?= exp

case_evalLogic6 = let j = object ["a" .= ("baz" :: Text)]
                      p = pure True
                      t = [RawText "foo", RawText "bar", Output $ Variable $ ObjectIndex "a" :| []]
                      res = evalLogic j p t
                      exp = AccSuccess ("foobarbaz", object ["a" .= ("baz" :: Text)])
                  in res @?= exp

case_evalLogic7 = let j = object []
                      p = pure True
                      t = [RawText "foo", RawText "bar", Output $ Variable $ ObjectIndex "a" :| []]
                      res = evalLogic j p t
                      exp = AccFailure [ JsonValueNotFound $ ObjectIndex "a" :| [] ]
                  in res @?= exp

--------------------------------------------------------------------------------
-- * renderTemplate
--------------------------------------------------------------------------------

case_renderTemplate1 = let j = object []
                           e = Output (Filter (QuoteString "abc") [FilterCell "toUpper" []])
                           res = renderTemplate j e
                           exp = AccSuccess ("ABC", j)
                       in res @?= exp

case_renderTemplate2 = let j = object []
                           e = Output (QuoteString "foo")
                           res = renderTemplate j e
                           exp = AccSuccess ("foo", j)
                       in res @?= exp

case_renderTemplate3 = let j = object ["a" .= ("baz" :: Text)]
                           e = Output (Variable $ ObjectIndex "a" :| [])
                           res = renderTemplate j e
                           exp = AccSuccess ("baz", object ["a" .= ("baz" :: Text)])
                       in res @?= exp

case_renderTemplate4 = let j = object []
                           e = Output (Variable $ ObjectIndex "a" :| [])
                           res = renderTemplate j e
                           exp = AccFailure [ JsonValueNotFound $ ObjectIndex "a" :| [] ]
                       in res @?= exp

case_renderTemplate5 = let j = object []
                           e = Output (Num $ sc 123)
                           res = renderTemplate j e
                           exp = AccSuccess ("123", j)
                       in res @?= exp

case_renderTemplate6 = let j = object []
                           e = RawText "foobar"
                           res = renderTemplate j e
                           exp = AccSuccess ("foobar", j)
                       in res @?= exp

case_renderTemplate7 = let j = object ["a" .= (1 :: Integer)]
                           e = IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                                       (TrueStatements [(RawText " foo ")])
                           res = renderTemplate j e
                           exp = AccSuccess (" foo ", object ["a" .= (1 :: Integer)])
                       in res @?= exp

case_renderTemplate8 = let j = object ["a" .= ("1" :: Text)]
                           e = IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                                       (TrueStatements [(RawText " foo ")])
                           res = renderTemplate j e
                           exp = AccSuccess ("", j)
                       in res @?= exp

case_renderTemplate9 = let j = object ["a" .= (1 :: Integer), "abc" .= ("yo" :: Text)]
                           e = IfLogic (IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                                       (TrueStatements [(Output $ Variable $ ObjectIndex "abc" :| []), (RawText " ok")]))
                                       (IfLogic Else (TrueStatements [RawText "yo"]))
                           res = renderTemplate j e
                           exp = AccSuccess ("yo ok", j)
                       in res @?= exp

case_renderTemplate10 = let j = object ["a" .= (2 :: Integer), "abc" .= ("yo" :: Text)]
                            e = IfLogic (IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                                        (TrueStatements [(Output $ Variable $ ObjectIndex "abc" :| []), (RawText " ok")]))
                                        (IfLogic Else (TrueStatements [RawText "yo"]))
                            res = renderTemplate j e
                            exp = AccSuccess ("yo", j)
                        in res @?= exp

case_renderTemplate11 = let j = object ["a" .= (2 :: Integer), "abc" .= ("yo" :: Text)]
                            e = IfLogic (IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                                    (TrueStatements [(Output $ Variable $ ObjectIndex "abc" :| [])]))
                                (IfLogic (IfLogic (ElsIfClause Trueth) (TrueStatements [RawText "ok"]))
                                         (IfLogic Else (TrueStatements [RawText "yo"])))
                            res = renderTemplate j e
                            exp = AccSuccess ("ok", j)
                        in res @?= exp

case_renderTemplate12 = let j = object ["a" .= (2 :: Integer), "abc" .= ("yo" :: Text)]
                            e = IfLogic (IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                                    (TrueStatements [(Output $ Variable $ ObjectIndex "abc" :| [] )]))
                                (IfLogic (IfLogic (ElsIfClause (Equal (Num $ sc 2) (Variable $ ObjectIndex "a" :| []))) (TrueStatements [RawText "ok"]))
                                         (IfLogic Else (TrueStatements [RawText "yo"])))
                            res = renderTemplate j e
                            exp = AccSuccess ("ok", j)
                        in res @?= exp

case_renderTemplate13 = let j = object ["a" .= (3 :: Integer), "abc" .= ("yo" :: Text)]
                            e = IfLogic (IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                                    (TrueStatements [(Output $ Variable $ ObjectIndex "abc" :| [])]))
                                (IfLogic (IfLogic (ElsIfClause (Equal (Num $ sc 2) (Variable $ ObjectIndex "a" :| []))) (TrueStatements [RawText "ok"]))
                                         (IfLogic Else (TrueStatements [RawText "yo", Output $ Variable $ ObjectIndex "abc" :| []])))
                            res = renderTemplate j e
                            exp = AccSuccess ("yoyo", j)
                        in res @?= exp

case_renderTemplate14 = let j = object ["a" .= (3 :: Integer), "ab" .= ("yo" :: Text)]
                            e = IfLogic (IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                                    (TrueStatements [(Output $ Variable $ ObjectIndex "abc" :| [])]))
                                (IfLogic (IfLogic (ElsIfClause (Equal (Num $ sc 2) (Variable $ ObjectIndex "a" :| []))) (TrueStatements [RawText "ok"]))
                                         (IfLogic Else (TrueStatements [RawText "yo", Output $ Variable $ ObjectIndex "abc" :| []])))
                            res = renderTemplate j e
                            exp = AccFailure [ JsonValueNotFound $ ObjectIndex "abc" :| [] ]
                        in res @?= exp

case_renderTemplate15 = let j = object ["a" .= (1 :: Integer), "ab" .= ("yo" :: Text)]
                            e = CaseLogic (Variable $ ObjectIndex "a" :| [])
                                          [(Num $ sc 1, TrueStatements [RawText "foo"])]
                            res = renderTemplate j e
                            exp = AccSuccess ("foo", j)
                        in res @?= exp

case_renderTemplate16 = let j = object ["a" .= (2 :: Integer), "ab" .= ("yo" :: Text)]
                            e = CaseLogic (Variable $ ObjectIndex "a" :| [])
                                           [(Num $ sc 1, TrueStatements [RawText "foo"])]
                            res = renderTemplate j e
                            exp = AccSuccess ("", j)
                        in res @?= exp

case_renderTemplate17 = let j = object ["a" .= (2 :: Integer), "ab" .= ("yo" :: Text)]
                            e = CaseLogic (Variable $ ObjectIndex "a" :| [])
                                          [(Num $ sc 1, TrueStatements [RawText "foo"])
                                          ,(Num $ sc 2, TrueStatements [RawText "baz"])
                                          ]
                            res = renderTemplate j e
                            exp = AccSuccess ("baz", j)
                        in res @?= exp

case_renderTemplate18 = let j = object ["a" .= (2 :: Integer), "ab" .= ("yo" :: Text)]
                            e = CaseLogic (Variable $ ObjectIndex "a" :| [])
                                          [ (Num $ sc 1, TrueStatements [RawText "foo"])
                                          , (Num $ sc 3, TrueStatements [RawText "baz"])
                                          , (Else, TrueStatements [RawText "quux"])
                                          ]
                            res = renderTemplate j e
                            exp = AccSuccess ("quux", j)
                        in res @?= exp

case_renderTemplate19 = let j = object ["a" .= (2 :: Integer), "ab" .= ("yo" :: Text)]
                            e = CaseLogic (Variable $ ObjectIndex "a" :| [])
                                          [ (Num $ sc 1, TrueStatements [RawText "foo"])
                                          , (Num $ sc 3, TrueStatements [RawText "baz"])
                                          , (Else, TrueStatements [Output $ Variable $ ObjectIndex "ab" :| []])
                                          ]
                            res = renderTemplate j e
                            exp = AccSuccess ("yo", j)
                        in res @?= exp

case_renderTemplate20 = let j = object []
                            e = Trueth
                            res = renderTemplate j e
                            exp = AccFailure [ LiquidError "Template rendering critical error!" ]
                        in res @?= exp

case_renderTemplate21 = let j = object ["a" .= (1 :: Integer)]
                            e = IfLogic (IfKeyClause (Variable $ ObjectIndex "a" :| []))
                                        (TrueStatements [(RawText " foo ")])
                            res = renderTemplate j e
                            exp = AccSuccess (" foo ", j)
                        in res @?= exp

case_renderTemplate22 = let j = object ["b" .= (1 :: Integer)]
                            e = IfLogic (IfKeyClause (Variable $ ObjectIndex "a" :| []))
                                        (TrueStatements [(RawText " foo ")])
                            res = renderTemplate j e
                            exp = AccSuccess ("", j)
                        in res @?= exp

case_renderTemplate23 = let j = object ["a" .= (3 :: Integer), "abc" .= ("yo" :: Text)]
                            e = IfLogic (IfLogic (IfKeyClause (Variable $ ObjectIndex "a" :| []))
                                    (TrueStatements [(Output $ Variable $ ObjectIndex "abc" :| [])]))
                                (IfLogic (IfLogic (ElsIfClause (Equal (Num $ sc 2) (Variable $ ObjectIndex "a" :| []))) (TrueStatements [RawText "ok"]))
                                         (IfLogic Else (TrueStatements [RawText "yo", Output $ Variable $ ObjectIndex "abc" :| []])))
                            res = renderTemplate j e
                            exp = AccSuccess ("yo", j)
                        in res @?= exp

case_renderTemplate24 = let j = object ["a" .= A.Null, "abc" .= ("yo" :: Text)]
                            e = IfLogic (IfLogic (IfKeyClause (Variable $ ObjectIndex "a" :| []))
                                    (TrueStatements [(Output $ Variable $ ObjectIndex "abc" :| [])]))
                                (IfLogic (IfLogic (ElsIfClause (Equal (Num $ sc 2) (Variable $ ObjectIndex "a" :| []))) (TrueStatements [RawText "ok"]))
                                         (IfLogic Else (TrueStatements [RawText "yohh", Output $ Variable $ ObjectIndex "abc" :| []])))
                            res = renderTemplate j e
                            exp = AccSuccess ("yohhyo", j)
                        in res @?= exp

case_renderTemplate25 = let j = object ["abc" .= ("yo" :: Text)]
                            e = IfLogic (IfLogic (IfKeyClause (Variable $ ObjectIndex "a" :| []))
                                    (TrueStatements [(Output $ Variable $ ObjectIndex "abc" :| [])]))
                                (IfLogic (IfLogic (ElsIfClause Falseth) (TrueStatements [RawText "ok"]))
                                         (IfLogic Else (TrueStatements [RawText "yohh", Output $ Variable $ ObjectIndex "abc" :| []])))
                            res = renderTemplate j e
                            exp = AccSuccess ("yohhyo", j)
                        in res @?= exp

case_renderTemplate26 = let j = object ["abc" .= ("yo" :: Text)]
                            e = IfLogic (IfLogic (IfKeyClause Null)
                                    (TrueStatements [(Output $ Variable $ ObjectIndex "abc" :| [])]))
                                (IfLogic (IfLogic (ElsIfClause Falseth) (TrueStatements [RawText "ok"]))
                                         (IfLogic Else (TrueStatements [RawText "yohh", Output $ Variable $ ObjectIndex "abc" :| []])))
                            res = renderTemplate j e
                            exp = AccFailure [ RenderingFailure "Can't evalulate if key on anything other than json context variables" ]
                        in res @?= exp

case_renderTemplate27 = let j = object []
                            e = AssignClause (Variable $ ObjectIndex "a" :| []) (QuoteString "foo")
                            res = renderTemplate j e
                            exp = AccSuccess (Data.Text.empty, object ["a" .= ("foo" :: Text)])
                        in res @?= exp

case_renderTemplate28 = let j = object ["a" .= (["b"] :: [Text])]
                            e = ForLogic (ForClause (Variable $ ObjectIndex "x" :| []) (Variable $ ObjectIndex "a" :| []))
                                         (TrueStatements [RawText " foo "])
                            res = renderTemplate j e
                            exp = AccSuccess (" foo ", object ["a" .= (["b"] :: [Text]), "x" .= ("b" :: Text)])
                        in res @?= exp

case_renderTemplate29 = let j = object ["a" .= (["b", "c"] :: [Text])]
                            e = ForLogic (ForClause (Variable $ ObjectIndex "x" :| []) (Variable $ ObjectIndex "a" :| []))
                                         (TrueStatements [RawText " foo "])
                            res = renderTemplate j e
                            exp = AccSuccess (" foo  foo ", object ["a" .= (["b", "c"] :: [Text]), "x" .= ("c" :: Text)])
                        in res @?= exp

--------------------------------------------------------------------------------
-- * interpret
--------------------------------------------------------------------------------

case_interpretWithJson1 = let j = maybe (object []) id (preview _Value ("{\"a\":\"foo\",\"b\":[\"hello\", 1, \"bar\"]}" :: Text))
                              t = "hello{{\'world\' | toUpper}}{%if a == \'foo\' %}{{b[0]}}{% endif %}"
                              res = interpretWithJson j t
                              exp = AccSuccess "helloWORLDhello"
                          in res @?= exp

case_interpretWithJson2 = let j = maybe (object []) id (preview _Value ("{\"a\":\"foo\",\"b\":[\"hello\", 1, \"bar\"]}" :: Text))
                              t = "hello{{\'world\' | toUpper}}{%if a == \'foo\' %} {{b[1]}}{% endif %}"
                              res = interpretWithJson j t
                              exp = AccSuccess "helloWORLD 1"
                          in res @?= exp

case_interpretWithJson3 = let j = maybe (object []) id (preview _Value ("{\"a\":\"foo\",\"b\":[\"hello\", 1, \"bar\"]}" :: Text))
                              t = "hello{{\'world\' | toTitle}}{%if a == \'foo\' %} {{a}}8afbj!=%% {% endif %}"
                              res = interpretWithJson j t
                              exp = AccFailure [ TemplateParsingError "hello{{\'world\' | toTitle}}"
                                                                      "{%if a == \'foo\' %} {{a}}8afbj!=%% {% endif %}"
                                                                      ["Syntax Error", "Block Parsing"]
                                               ]
                          in res @?= exp

case_interpretWithJson4 = let j = maybe (object []) id (preview _Value ("{\"a\":\"foo\",\"b\":[\"hello\", 1, \"bar\"]}" :: Text))
                              t = "hello{{\'world\' | toTitle:\'a\'}}{%if a == \'foo\' %} {{a}}{% endif %}"
                              res = interpretWithJson j t
                              exp = AccFailure [ TemplateParsingError "hello"
                                                                      "{{\'world\' | toTitle:\'a\'}}{%if a == \'foo\' %} {{a}}{% endif %}"
                                                                      ["Syntax Error", "Block Parsing"]
                                               ]
                          in res @?= exp

case_interpretWithJson5 = let j = maybe (object []) id (preview _Value ("{\"a\":\"foo\",\"b\":[\"hello\", 1, \"bar\"]}" :: Text))
                              t = "hello{{ b | first | toUpper }} yoyoyo"
                              res = interpretWithJson j t
                              exp = AccSuccess "helloHELLO yoyoyo"
                          in res @?= exp

case_interpretWithJson6 = let j = maybe (object []) id (preview _Value ("{\"a\":\"foo\",\"b\":[]}" :: Text))
                              t = "hello{{ b | lastOrDefault:\'barry\' | toUpper }} yoyoyo"
                              res = interpretWithJson j t
                              exp = AccSuccess "helloBARRY yoyoyo"
                          in res @?= exp

-- | n.b. Must remain at the bottom of the file for TH
rendererTests :: TestTree
rendererTests = $(testGroupGenerator)

