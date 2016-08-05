{-# LANGUAGE TemplateHaskell #-}

module Text.Liquid.HelperTests where

import           Control.Lens             hiding ((.=))
import           Data.Aeson
import           Data.List.NonEmpty
import           Data.Scientific
import           Data.Text                (Text, empty, null, pack)
import           Prelude                  hiding (null)
import           Test.QuickCheck          hiding (output)
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck    hiding (output)
import           Test.Tasty.TH
import           Text.Liquid.Generators   (genExpr)
import           Text.Liquid.Helpers
import           Text.Liquid.Types

--------------------------------------------------------------------------------
-- * foldM'
--------------------------------------------------------------------------------

case_foldM1 = let fn = (\a b -> return $ a + b)
                  a  = 1
                  bs = [2,3,4,5]
                  exp = Just 15
                  res = foldM' fn a bs
              in res @?= exp

case_foldM2 = let fn = (\a b -> Nothing)
                  a  = 1
                  bs = [2,3,4,5]
                  exp = Nothing
                  res = foldM' fn a bs
              in res @?= exp

--------------------------------------------------------------------------------
-- * formatNum
--------------------------------------------------------------------------------

-- | Test helper for scientific values
sc :: Double -> Scientific
sc d = fromFloatDigits d

case_formatNum1 = formatNum (sc 123) @?= "123"

case_formatNum2 = formatNum (sc 123.456) @?= "123.456"

case_formatNum3 = formatNum (sc 123.0) @?= "123"

--------------------------------------------------------------------------------
-- * buildLens
--------------------------------------------------------------------------------

case_buildLens1 = let inputPath  = ObjectIndex "a" :| []
                      testObject = object ["a" .= ("foo" :: Text)]
                      resLens = buildLens inputPath
                  in testObject ^? resLens @?= Just "foo"

case_buildLens2 = let inputPath  = ObjectIndex "a" :| [ObjectIndex "b"]
                      testObject = object ["a" .= (object ["b" .= ("foo" :: Text)])]
                      resLens = buildLens inputPath
                  in testObject ^? resLens @?= Just "foo"

case_buildLens3 = let inputPath  = ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 1]
                      testObject = object ["a" .= (object ["b" .= (["foo", "bar"] :: [Text])])]
                      resLens = buildLens inputPath
                  in testObject ^? resLens @?= Just "bar"

--------------------------------------------------------------------------------
-- * renderExpr
--------------------------------------------------------------------------------

prop_renderExpr1 =
  forAll genExpr (\e -> (not . null $ renderExpr e))

-- | n.b. Must remain at the bottom of the file for TH
helperTests :: TestTree
helperTests = $(testGroupGenerator)

