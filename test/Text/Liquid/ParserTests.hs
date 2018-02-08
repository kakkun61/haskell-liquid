{-# LANGUAGE TemplateHaskell #-}

module Text.Liquid.ParserTests where

import           Control.Lens             hiding (elements)
import           Data.Attoparsec.Text
import           Data.List.NonEmpty
import           Data.Monoid
import           Data.Scientific
import           Data.Text                (Text, pack, strip, unpack)
import           Test.QuickCheck          hiding (output)
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck    hiding (output)
import           Test.Tasty.TH
import           Text.Liquid.Generators
import           Text.Liquid.Parser
import           Text.Liquid.Tokens
import           Text.Liquid.Types


--------------------------------------------------------------------------------
-- * Between
--------------------------------------------------------------------------------

case_between_1 = parseOnly (between tagStart tagEnd ifStart) "{%if%}" @?= Right "if"
case_between_2 = parseOnly (between tagStart tagEnd ifStart) "{% if%}" @?= Left "string"
case_between_3 = parseOnly (between tagStart tagEnd ifStart) "{%if %}" @?= Left "Failed reading: Tag or output statement incomplete"
case_between_4 = parseOnly (between tagStart tagEnd ifStart) "{% if %}" @?= Left "string"
case_between_5 = parseOnly (between tagStart tagEnd ifStart) "{% if " @?= Left "string"
case_between_6 = parseOnly (between tagStart tagEnd ifStart) "{%if " @?= Left "Failed reading: Tag or output statement incomplete"

--------------------------------------------------------------------------------
-- * Stripped
--------------------------------------------------------------------------------

case_stripped_1 = parseOnly (stripped ifStart) "if" @?= Right "if"
case_stripped_2 = parseOnly (stripped ifStart) " if" @?= Right "if"
case_stripped_3 = parseOnly (stripped ifStart) "if " @?= Right "if"
case_stripped_4 = parseOnly (stripped ifStart) " if " @?= Right "if"

--------------------------------------------------------------------------------
-- * Tag
--------------------------------------------------------------------------------

case_tag_1 = parseOnly (tag ifStart) "{%if%}j" @?= Right "if"
case_tag_2 = parseOnly (tag ifStart) "{% if%}" @?= Right "if"
case_tag_3 = parseOnly (tag ifStart) "{%if %}" @?= Right "if"
case_tag_4 = parseOnly (tag ifStart) "{% if %}" @?= Right "if"
case_tag_5 = parseOnly (tag ifStart) "{% if %}hello" @?= Right "if"
case_tag_6 = parseOnly (tag ifStart) "foo{{ if }}" @?= Left "string"
case_tag_7 = parseOnly (tag ifStart) "foo{{ if }}bar" @?= Left "string"

--------------------------------------------------------------------------------
-- * OutputTag
--------------------------------------------------------------------------------

case_outputTag_1 = parseOnly (outputTag ifStart) "{{if}}j" @?= Right "if"
case_outputTag_2 = parseOnly (outputTag ifStart) "{{ if}}" @?= Right "if"
case_outputTag_3 = parseOnly (outputTag ifStart) "{{if }}" @?= Right "if"
case_outputTag_4 = parseOnly (outputTag ifStart) "{{ if }}" @?= Right "if"
case_outputTag_5 = parseOnly (outputTag ifStart) "{{ if }}bar" @?= Right "if"
case_outputTag_6 = parseOnly (outputTag ifStart) "foo{{ if }}" @?= Left "string"
case_outputTag_7 = parseOnly (outputTag ifStart) "foo{{ if }}bar" @?= Left "string"

--------------------------------------------------------------------------------
-- * Var
--------------------------------------------------------------------------------

case_var_1 = parseOnly var "hello" @?= Right "hello"
case_var_2 = parseOnly var "he_llo" @?= Right "he_llo"
case_var_3 = parseOnly var "_hello" @?= Right "_hello"
case_var_4 = parseOnly var "hello_" @?= Right "hello_"
case_var_5 = parseOnly var "_hello_" @?= Right "_hello_"
case_var_6 = parseOnly var "-hello" @?= Right "-hello"
case_var_7 = parseOnly var "hello-" @?= Right "hello-"
case_var_8 = parseOnly var "-hello-" @?= Right "-hello-"
case_var_9 = parseOnly var "-hell-o-" @?= Right "-hell-o-"
prop_var_1 = \(VariableChars t) -> (parseOnly var t == Right t)

--------------------------------------------------------------------------------
-- * ParseBoxedInt
--------------------------------------------------------------------------------

case_parseBoxedInt_1 = parseOnly parseBoxedInt "[0]" @?= Right 0
case_parseBoxedInt_2 = parseOnly parseBoxedInt "[1]" @?= Right 1
case_parseBoxedInt_3 = parseOnly parseBoxedInt "1]" @?= Left "'[': Failed reading: satisfy"
case_parseBoxedInt_4 = parseOnly parseBoxedInt "[1.0]" @?= Right 1
case_parseBoxedInt_5 = parseOnly parseBoxedInt "[a]" @?= Left "Failed reading: takeWhile1"
case_parseBoxedInt_6 = parseOnly parseBoxedInt "[-2]" @?=
  Left "Failed reading: invalid variable (array) index, expecting a positive integer"

--------------------------------------------------------------------------------
-- * TagWith
--------------------------------------------------------------------------------

case_tagWith_1 = parseOnly (tagWith ifStart ifStart) "{% if if %}" @?= Right "if"
case_tagWith_2 = parseOnly (tagWith ifStart els) "{% if else %}" @?= Right "else"
case_tagWith_3 = parseOnly (tagWith ifStart ifStart) "{%if if%}" @?= Right "if"
case_tagWith_4 = parseOnly (tagWith ifStart els) "{%if else%}" @?= Right "else"
case_tagWith_5 = parseOnly (tagWith ifStart ifStart) "{%if if %}" @?= Right "if"
case_tagWith_6 = parseOnly (tagWith ifStart ifStart) "{%if if %}" @?= Right "if"
case_tagWith_7 = parseOnly (tagWith ifStart ifStart) "{%if i %}" @?= Left "string"
case_tagWith_8 = parseOnly (tagWith ifStart ifStart) "{%i if %}" @?= Left "string"
case_tagWith_9 = parseOnly (tagWith ifStart ifStart) "{% %}" @?= Left "string"

--------------------------------------------------------------------------------
-- * VarIndexSection
--------------------------------------------------------------------------------

case_varIndexSection_1 = parseOnly varIndexSection "a" @?=
  Right (ObjectIndex "a" :| [])

case_varIndexSection_2 = parseOnly varIndexSection "a.b" @?=
  Right (ObjectIndex "a" :| [ObjectIndex "b"])

case_varIndexSection_3 = parseOnly varIndexSection "a.b.c" @?=
  Right (ObjectIndex "a" :| [ObjectIndex "b", ObjectIndex "c"])

case_varIndexSection_4 = parseOnly varIndexSection "a.b[1]" @?=
  Right (ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 1])

case_varIndexSection_5 = parseOnly varIndexSection "a.b[1][2]" @?=
  Right (ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 1, ArrayIndex 2])

case_varIndexSection_6 = parseOnly varIndexSection "a.b[2].c" @?=
  Right (ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 2])

case_varIndexSection_7 = parseOnly varIndexSection "a.b[2][3].c" @?=
  Right (ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 2, ArrayIndex 3])

case_varIndexSection_8 = parseOnly varIndexSection "a.b[c]" @?=
  Left "Failed reading: invalid array index - ill-typed"

--------------------------------------------------------------------------------
-- * Variable
--------------------------------------------------------------------------------

case_variable_1 = parseOnly variable "a" @?=
  Right (Variable $ ObjectIndex "a" :| [])

case_variable_2 = parseOnly variable "a.b" @?=
  Right (Variable $ ObjectIndex "a" :| [ObjectIndex "b"])

case_variable_3 = parseOnly variable "a.b.c" @?=
  Right (Variable $ ObjectIndex "a" :| [ObjectIndex "b", ObjectIndex "c"])

case_variable_4 = parseOnly variable "a.b[1]" @?=
  Right (Variable $ ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 1])

case_variable_5 = parseOnly variable "a.b[c].d" @?=
  Left "Failed reading: invalid array index - ill-typed"

case_variable_6 = parseOnly variable "a.b[2].c" @?=
  Right (Variable $ ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 2, ObjectIndex "c"])

case_variable_7 = parseOnly variable "a.b[3].[2]" @?=
  Right (Variable $ ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 3])

case_variable_8 = parseOnly variable "a.b[3].c[4]" @?=
  Right (Variable $ ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 3, ObjectIndex "c", ArrayIndex 4])

case_variable_9 = parseOnly variable "a.b[3][1].c[4]" @?=
  Right (Variable $ ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 3, ArrayIndex 1, ObjectIndex "c", ArrayIndex 4])

case_variable_10 = parseOnly variable "[1]" @?=
  Left "Failed reading: invalid var index section"

case_variable_11 = parseOnly variable "[1][0]" @?=
  Left "Failed reading: invalid var index section"

case_variable_12 = parseOnly variable "  " @?=
  Left "Failed reading: invalid var index section"

--------------------------------------------------------------------------------
-- * RawTag
--------------------------------------------------------------------------------

prop_rawTag_1 = \(VariableChars t) -> (parseOnly rawTag ("{% raw %} " <> t <> " {% endraw %}")) == Right (RawText t)
prop_rawTag_2 = \(VariableChars t) -> (parseOnly rawTag ("{% raw %}" <> t <> "{% endraw %}")) == Right (RawText t)
prop_rawTag_3 = \(AnyChar t) -> (parseOnly rawTag ("{% raw %} " <> t <> " {% endraw %}")) == Right (RawText (strip t))
prop_rawTag_4 = \(AnyChar t) -> (parseOnly rawTag ("{% raw %}" <> t <> "{% endraw %}")) == Right (RawText (strip t))

--------------------------------------------------------------------------------
-- * Comment
--------------------------------------------------------------------------------

prop_comment_1 = \(VariableChars t) ->
  (parseOnly commentTag ("{% comment %} " <> t <> " {% endcomment %}")) == (Right Noop :: Either String Expr)

prop_comment_2 = \(VariableChars t) ->
  (parseOnly commentTag ("{% comment %}" <> t <> "{% endcomment %}")) == (Right Noop :: Either String Expr)

prop_comment_3 = \(AnyChar t) ->
  (parseOnly commentTag ("{% comment %} " <> t <> " {% endcomment %}")) == (Right Noop :: Either String Expr)

prop_comment_4 = \(AnyChar t) ->
  (parseOnly commentTag ("{% comment %}" <> t <> "{% endcomment %}")) == (Right Noop :: Either String Expr)

--------------------------------------------------------------------------------
-- * TextPart
--------------------------------------------------------------------------------

case_textPart_1 = parseOnly textPart "foo{{" @?= Right (RawText "foo")
case_textPart_2 = parseOnly textPart "foo{%" @?= Right (RawText "foo")
case_textPart_3 = parseOnly textPart "foo {" @?= Left "Failed reading: satisfy"
prop_textPart_1 = \(VariableChars t) -> (parseOnly textPart t == Right (RawText t))

--------------------------------------------------------------------------------
-- * QuoteString
--------------------------------------------------------------------------------

case_quoteString_1 = parseOnly quoteString "\'foo\'" @?= (Right $ QuoteString "foo")
case_quoteString_2 = parseOnly quoteString "\'foo" @?= Left "not enough input"
case_quoteString_3 = parseOnly quoteString "\"foo\"" @?= (Right $ QuoteString "foo")
case_quoteString_4 = parseOnly quoteString "\'foo" @?= Left "not enough input"
case_quoteString_5 = parseOnly quoteString "\'foo\'\'" @?= (Right $ QuoteString "foo")
case_quoteString_6 = parseOnly quoteString "foo\'" @?= Left "Failed reading: satisfy"
case_quoteString_7 = parseOnly quoteString "\'\'" @?= Right (QuoteString "")

--------------------------------------------------------------------------------
-- * Binary Predicate
--------------------------------------------------------------------------------

case_binaryPredicate_1 = parseOnly binaryPredicate "\'foo\' == \'baz\'" @?=
  Right (Equal (QuoteString "foo") (QuoteString "baz"))

case_binaryPredicate_2 = parseOnly binaryPredicate "\'foo\' != \'baz\'" @?=
  Right (NotEqual (QuoteString "foo") (QuoteString "baz"))

case_binaryPredicate_3 = parseOnly binaryPredicate "\'foo\' >= \'baz\'" @?=
  Right (GtEqual (QuoteString "foo") (QuoteString "baz"))

case_binaryPredicate_4 = parseOnly binaryPredicate "\'foo\' <= \'baz\'" @?=
  Right (LtEqual (QuoteString "foo") (QuoteString "baz"))

case_binaryPredicate_5 = parseOnly binaryPredicate "\'foo\' > \'baz\'" @?=
  Right (Gt (QuoteString "foo") (QuoteString "baz"))

case_binaryPredicate_6 = parseOnly binaryPredicate "\'foo\' < \'baz\'" @?=
  Right (Lt (QuoteString "foo") (QuoteString "baz"))

case_binaryPredicate_7 = parseOnly binaryPredicate "\'foo\' == 123" @?=
  Right (Equal (QuoteString "foo") (Num $ sc 123))

case_binaryPredicate_8 = parseOnly binaryPredicate "\'foo\' == abc" @?=
  Right (Equal (QuoteString "foo") (Variable $ ObjectIndex "abc" :| []))

case_binaryPredicate_9 = parseOnly binaryPredicate "\'foo\' == abc.d" @?=
  Right (Equal (QuoteString "foo") (Variable $ ObjectIndex "abc" :| [ObjectIndex "d"]))

case_binaryPredicate_10 = parseOnly binaryPredicate "abc == abc.d" @?=
  Right (Equal (Variable $ ObjectIndex "abc" :| []) (Variable $ ObjectIndex "abc" :| [ObjectIndex "d"]))

case_binaryPredicate_11 = parseOnly binaryPredicate "abc[1] == abc.d[2]" @?=
  Right (Equal (Variable $ ObjectIndex "abc" :| [ArrayIndex 1]) (Variable $ ObjectIndex "abc" :| [ObjectIndex "d", ArrayIndex 2]))

case_binaryPredicate_12 = parseOnly binaryPredicate "abc[1].def == abc.d[2]" @?=
  Right (Equal (Variable $ ObjectIndex "abc" :| [ArrayIndex 1, ObjectIndex "def"])
               (Variable $ ObjectIndex "abc" :| [ObjectIndex "d", ArrayIndex 2]))

case_binaryPredicate_13 = parseOnly binaryPredicate "abc[1].def != \'yo\'" @?=
  Right (NotEqual (Variable $ ObjectIndex "abc" :| [ArrayIndex 1, ObjectIndex "def"])
                  (QuoteString "yo"))

case_binaryPredicate_14 = parseOnly binaryPredicate "abc[1].def != yo\'" @?=
  Right (NotEqual (Variable $ ObjectIndex "abc" :| [ArrayIndex 1, ObjectIndex "def"])
                  (Variable $ ObjectIndex "yo" :| []))

case_binaryPredicate_15 = parseOnly binaryPredicate "\'foo\'==\'baz\'" @?=
  Right (Equal (QuoteString "foo") (QuoteString "baz"))

case_binaryPredicate_16 = parseOnly binaryPredicate "\'foo\'!=abc.d" @?=
  Right (NotEqual (QuoteString "foo") (Variable $ ObjectIndex "abc" :| [ObjectIndex "d"]))

case_binaryPredicate_17 = parseOnly binaryPredicate "\'foo\' == false" @?=
  Right (Equal (QuoteString "foo") Falseth)

case_binaryPredicate_18 = parseOnly binaryPredicate "true == foo" @?=
  Right (Equal Trueth (Variable $ ObjectIndex "foo" :| []))

case_binaryPredicate_19 = parseOnly binaryPredicate "true == null" @?=
  Right (Equal Trueth Null)

case_binaryPredicate_20 = parseOnly binaryPredicate "nil != 'barry'" @?=
  Right (NotEqual Nil (QuoteString "barry"))

--------------------------------------------------------------------------------
-- * Truthy
--------------------------------------------------------------------------------

case_truthy1 = parseOnly truthy "nil" @?= Right Nil
case_truthy2 = parseOnly truthy "null" @?= Right Null
case_truthy3 = parseOnly truthy "nul" @?= Right (Truthy $ Variable $ ObjectIndex "nul" :| [])
case_truthy4 = parseOnly truthy "false" @?= Right Falseth
case_truthy5 = parseOnly truthy "true" @?= Right Trueth
case_truthy6 = parseOnly truthy "123" @?= Right (Truthy (Num $ sc 123))
case_truthy7 = parseOnly truthy "3.142" @?= Right (Truthy (Num $ sc 3.142))
case_truthy8 = parseOnly truthy "\'hello\'" @?= Right (Truthy (QuoteString "hello"))
case_truthy9 = parseOnly truthy "\'false\'" @?= Right (Truthy (QuoteString "false")) -- n.b. this is a quoted string, and therefore truthy
case_truthy10 = parseOnly truthy "a.b[123]" @?= Right (Truthy $ Variable $ ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 123])

--------------------------------------------------------------------------------
-- * Predicate
--------------------------------------------------------------------------------

case_predicate1 = parseOnly predicate "1 > 2 or 3 == 3" @?=
  Right (Or (Gt (Num $ sc 1) (Num $ sc 2)) (Equal (Num $ sc 3) (Num $ sc 3)))

case_predicate2 = parseOnly predicate "1 > 2or3 == 3" @?=
  Right (Or (Gt (Num $ sc 1) (Num $ sc 2)) (Equal (Num $ sc 3) (Num $ sc 3)))

case_predicate3 = parseOnly predicate "1 > 2 and 3 == 3" @?=
  Right (And (Gt (Num $ sc 1) (Num $ sc 2)) (Equal (Num $ sc 3) (Num $ sc 3)))

case_predicate4 = parseOnly predicate "1> 2and3 != 3" @?=
  Right (And (Gt (Num $ sc 1) (Num $ sc 2)) (NotEqual (Num $ sc 3) (Num $ sc 3)))

case_predicate5 = parseOnly predicate "1>2and3 != 3" @?=
  Right (And (Gt (Num $ sc 1) (Num $ sc 2)) (NotEqual (Num $ sc 3) (Num $ sc 3)))

case_predicate6 = parseOnly predicate "1<2and3!=3" @?=
  Right (And (Lt (Num $ sc 1) (Num $ sc 2)) (NotEqual (Num $ sc 3) (Num $ sc 3)))

case_predicate7 = parseOnly predicate "1<2" @?= Left "not enough input"

--------------------------------------------------------------------------------
-- * Predicate Clause
--------------------------------------------------------------------------------

case_predicateClause1 = parseOnly predicateClause "1 > 2 or 3 == 3" @?=
  Right (Or (Gt (Num $ sc 1) (Num $ sc 2)) (Equal (Num $ sc 3) (Num $ sc 3)))

case_predicateClause2 = parseOnly predicateClause "1 > 2" @?=
  Right (Gt (Num $ sc 1) (Num $ sc 2))

case_predicateClause3 = parseOnly predicateClause "a.b > 2" @?=
  Right (Gt (Variable $ ObjectIndex "a" :| [ObjectIndex "b"]) (Num $ sc 2))

case_predicateClause4 = parseOnly predicateClause "a.b" @?=
  Right (Truthy (Variable $ ObjectIndex "a" :| [ObjectIndex "b"]))

case_predicateClause5 = parseOnly predicateClause "\'a\'" @?= Right (Truthy (QuoteString "a"))
case_predicateClause6 = parseOnly predicateClause "false" @?= Right Falseth

--------------------------------------------------------------------------------
-- * If Clause
--------------------------------------------------------------------------------

case_ifClause1 = parseOnly ifClause "{% if a.b %}" @?=
  Right (IfClause (Truthy (Variable $ ObjectIndex "a" :| [ObjectIndex "b"])))

case_ifClause2 = parseOnly ifClause "{% ifa.b %}" @?=
  Right (IfClause (Truthy (Variable $ ObjectIndex "a" :| [ObjectIndex "b"])))

case_ifClause3 = parseOnly ifClause "{%ifa.b %}" @?=
  Right (IfClause (Truthy (Variable $ ObjectIndex "a" :| [ObjectIndex "b"])))

case_ifClause4 = parseOnly ifClause "{%iff.b %}" @?=
  Right (IfClause (Truthy (Variable $ ObjectIndex "f" :| [ObjectIndex "b"])))

case_ifClause5 = parseOnly ifClause "{% if \'a\' %}" @?=
  Right (IfClause (Truthy (QuoteString "a")))

case_ifClause6 = parseOnly ifClause "{% if\'a\' %}" @?=
  Right (IfClause (Truthy (QuoteString "a")))

case_ifClause7 = parseOnly ifClause "{% if \'a\' == a %}" @?=
  Right (IfClause (Equal (QuoteString "a") (Variable $ ObjectIndex "a" :| [])))

case_ifClause8 = parseOnly ifClause "{% if \'a\'==a%}" @?=
  Right (IfClause (Equal (QuoteString "a") (Variable $ ObjectIndex "a" :| [])))

case_ifClause9 = parseOnly ifClause "{% ifa!=\'a\'%}" @?=
  Right (IfClause (NotEqual (Variable $ ObjectIndex "a" :| []) (QuoteString "a")))

case_ifClause10 = parseOnly ifClause "{% if a.b[1] != \'a\'%}" @?=
  Right (IfClause (NotEqual (Variable $ ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 1]) (QuoteString "a")))

case_ifClause11 = parseOnly ifClause "{% if a.b[1] < 14.56 %}" @?=
  Right (IfClause (Lt (Variable $ ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 1]) (Num $ sc 14.56)))

case_ifClause12 = parseOnly ifClause "{% if 14.56 > a.b[1] %}" @?=
  Right (IfClause (Gt (Num $ sc 14.56) (Variable $ ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 1])))

case_ifClause13 = parseOnly ifClause "{% if a contains \'foo\' %}" @?=
  Right (IfClause (Contains (Variable $ ObjectIndex "a" :| []) (QuoteString "foo")))

case_ifClause14 = parseOnly ifClause "{% if a.b contains 123 %}" @?=
  Right (IfClause (Contains (Variable $ ObjectIndex "a" :| [ObjectIndex "b"]) (Num $ sc 123)))

case_ifClause15 = parseOnly ifClause "{% if a.b[2]contains 123 %}" @?=
  Right (IfClause (Contains (Variable $ ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 2]) (Num $ sc 123)))

case_ifClause16 = parseOnly ifClause "{% if acontains \'foo\' %}" @?=
  Left "Failed reading: Tag or output statement incomplete"

--------------------------------------------------------------------------------
-- * IfKey Clause
--------------------------------------------------------------------------------

case_ifKeyClause1 = parseOnly ifKeyClause "{% ifkey a.b %}" @?=
  Right (IfKeyClause (Variable $ ObjectIndex "a" :| [ObjectIndex "b"]))

case_ifKeyClause2 = parseOnly ifKeyClause "{% ifkey null %}" @?=
  Left "Failed reading: Only variables as ifkey args allowed"

case_ifKeyClause3 = parseOnly ifKeyClause "{% ifkey nil %}" @?=
  Left "Failed reading: Only variables as ifkey args allowed"

case_ifKeyClause4 = parseOnly ifKeyClause "{% ifkey true %}" @?=
  Left "Failed reading: Only variables as ifkey args allowed"

case_ifKeyClause5 = parseOnly ifKeyClause "{% ifkey false %}" @?=
  Left "Failed reading: Only variables as ifkey args allowed"

case_ifKeyClause6 = parseOnly ifKeyClause "{% ifkey  %}" @?=
  Left "Failed reading: invalid var index section"

case_ifKeyClause7 = parseOnly ifKeyClause "{% ifkey key%}" @?=
  Right (IfKeyClause (Variable $ ObjectIndex "key" :| []))

case_ifKeyClause8 = parseOnly ifKeyClause "{% ifkey key[10]%}" @?=
  Right (IfKeyClause (Variable $ ObjectIndex "key" :| [ArrayIndex 10]))

--------------------------------------------------------------------------------
-- * Else If Clause
--------------------------------------------------------------------------------
case_elsifClause1 = parseOnly elsifClause "{% elsif a.b %}" @?=
  Right (ElsIfClause (Truthy (Variable $ ObjectIndex "a" :| [ObjectIndex "b"])))

case_elsifClause2 = parseOnly elsifClause "{% elsifa.b %}" @?=
  Right (ElsIfClause (Truthy (Variable $ ObjectIndex "a" :| [ObjectIndex "b"])))

case_elsifClause3 = parseOnly elsifClause "{%elsifa.b %}" @?=
  Right (ElsIfClause (Truthy (Variable $ ObjectIndex "a" :| [ObjectIndex "b"])))

case_elsifClause4 = parseOnly elsifClause "{%elsiff.b %}" @?=
  Right (ElsIfClause (Truthy (Variable $ ObjectIndex "f" :| [ObjectIndex "b"])))

case_elsifClause5 = parseOnly elsifClause "{% elsif \'a\' %}" @?=
  Right (ElsIfClause  (Truthy (QuoteString "a")))

case_elsifClause6 = parseOnly elsifClause "{% elsif\'a\' %}" @?=
  Right (ElsIfClause  (Truthy (QuoteString "a")))

case_elsifClause7 = parseOnly elsifClause "{% elsif \'a\' == a %}" @?=
  Right (ElsIfClause (Equal (QuoteString "a") (Variable $ ObjectIndex "a" :| [])))

case_elsifClause8 = parseOnly elsifClause "{% elsif \'a\'==a%}" @?=
  Right (ElsIfClause (Equal (QuoteString "a") (Variable $ ObjectIndex "a" :| [])))

case_elsifClause9 = parseOnly elsifClause "{% elsifa!=\'a\'%}" @?=
  Right (ElsIfClause (NotEqual (Variable $ ObjectIndex "a" :| []) (QuoteString "a")))

case_elsifClause10 = parseOnly elsifClause "{% elsif a.b[1] != \'a\'%}" @?=
  Right (ElsIfClause (NotEqual (Variable $ ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 1])
                               (QuoteString "a")))

case_elsifClause11 = parseOnly elsifClause "{% elsif a.b[1] < 14.56 %}" @?=
  Right (ElsIfClause (Lt (Variable $ ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 1])
                         (Num $ sc 14.56)))

case_elsifClause12 = parseOnly elsifClause "{% elsif 14.56 > a.b[1] %}" @?=
  Right (ElsIfClause (Gt (Num $ sc 14.56) (Variable $ ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 1])))

--------------------------------------------------------------------------------
-- * Else Clause
--------------------------------------------------------------------------------

case_elseClause1 = parseOnly elseClause "{% else %}" @?= Right Else
case_elseClause2 = parseOnly elseClause "{%else%}" @?= Right Else
case_elseClause3 = parseOnly elseClause "{%els%}" @?= Left "string"
case_elseClause4 = parseOnly elseClause "{%else" @?= Left "Failed reading: Tag or output statement incomplete"

--------------------------------------------------------------------------------
-- * End If Clause
--------------------------------------------------------------------------------

case_endIfClause1 = parseOnly endIfClause "{% endif %}" @?= Right Noop
case_endIfClause2 = parseOnly endIfClause "{%endif%}" @?= Right Noop
case_endIfClause3 = parseOnly endIfClause "{%endi%}" @?= (Left "string" :: Either String Expr)
case_endIfClause4 = parseOnly endIfClause "{%endif" @?= (Left "Failed reading: Tag or output statement incomplete"  :: Either String Expr)

--------------------------------------------------------------------------------
-- * Case Clause
--------------------------------------------------------------------------------

case_caseClause1 = parseOnly caseClause "{% case a %}" @?= Right (Variable $ ObjectIndex "a" :| [])
case_caseClause2 = parseOnly caseClause "{%case a%}" @?= Right (Variable $ ObjectIndex "a" :| [])
case_caseClause3 = parseOnly caseClause "{% case a.bc[2] %}" @?= Right (Variable $ ObjectIndex "a" :| [ObjectIndex "bc", ArrayIndex 2])
case_caseClause4 = parseOnly caseClause "{% case%}" @?= Left "Failed reading: invalid var index section"
case_caseClause5 = parseOnly caseClause "{% case a" @?= Left "Failed reading: Tag or output statement incomplete"
case_caseClause6 = parseOnly caseClause "{% %}" @?= Left "string"

--------------------------------------------------------------------------------
-- * When Clause (from case block)
--------------------------------------------------------------------------------

case_whenClause1 = parseOnly whenClause "{% when \'a\' %}" @?= Right (QuoteString "a")
case_whenClause2 = parseOnly whenClause "{% when\'a\'%}" @?= Right (QuoteString "a")
case_whenClause3 = parseOnly whenClause "{%when\'a\'%}" @?= Right (QuoteString "a")
case_whenClause4 = parseOnly whenClause "{%when\'a\' %}" @?= Right (QuoteString "a")
case_whenClause5 = parseOnly whenClause "{%when123 %}" @?= Right (Num $ sc 123)
case_whenClause6 = parseOnly whenClause "{% when %}" @?= Left "Failed reading: takeWhile1"

--------------------------------------------------------------------------------
-- * Assign Clause
--------------------------------------------------------------------------------

case_assignClause1 = parseOnly assignClause "{% assign a = 1 %}" @?= Right (AssignClause (Variable $ ObjectIndex "a" :| []) (Num 1))

--------------------------------------------------------------------------------
-- * Filter Name
--------------------------------------------------------------------------------

prop_filterName1 = \(AlphaChars t) -> (parseOnly filterName (t <> "|") == Right t)
prop_filterName2 = \(AlphaChars t) -> (parseOnly filterName (t <> " |") == Right t)
prop_filterName3 = \(AlphaChars t) -> (parseOnly filterName (t <> ":") == Right t)
prop_filterName4 = \(AlphaChars t) -> (parseOnly filterName (t <> " :") == Right t)

--------------------------------------------------------------------------------
-- * Filter Args
--------------------------------------------------------------------------------

case_filterArgs1 = parseOnly filterArgs "\'a\'" @?= Right [QuoteString "a"]
case_filterArgs2 = parseOnly filterArgs "\'a\',\'b\'" @?= Right [QuoteString "a", QuoteString "b"]
case_filterArgs3 = parseOnly filterArgs "\'a\', \'b\'" @?= Right [QuoteString "a", QuoteString "b"]
case_filterArgs4 = parseOnly filterArgs " \'a\', \'b\'" @?= Right [QuoteString "a", QuoteString "b"]
case_filterArgs5 = parseOnly filterArgs " \'a\', \'b\' " @?= Right [QuoteString "a", QuoteString "b"]
case_filterArgs6 = parseOnly filterArgs "1,\'b\'" @?= Right [Num $ sc 1, QuoteString "b"]
case_filterArgs7 = parseOnly filterArgs "\'a\',2" @?= Right [QuoteString "a", Num $ sc 2]
case_filterArgs8 = parseOnly filterArgs "123.0, 2" @?= Right [Num $ sc 123.0, Num $ sc 2]

--------------------------------------------------------------------------------
-- * Filter Cell
--------------------------------------------------------------------------------

case_filterCell1 = parseOnly filterCell "toUpper " @?= Right (FilterCell "toUpper" [])
case_filterCell2 = parseOnly filterCell "toUpper |" @?= Right (FilterCell "toUpper" [])
case_filterCell3 = parseOnly filterCell "toUpper|" @?= Right (FilterCell "toUpper" [])
case_filterCell4 = parseOnly filterCell "countElements " @?= Right (FilterCell "countElements" [])

case_filterCell5 = parseOnly filterCell "replace: \'foo\', \'bar\' " @?=
  Right (FilterCell "replace" [QuoteString "foo", QuoteString "bar"])

case_filterCell6 = parseOnly filterCell "replace:\'foo\',\'bar\' " @?=
  Right (FilterCell "replace" [QuoteString "foo", QuoteString "bar"])

case_filterCell7 = parseOnly filterCell "replace : \'bar\' " @?=
  Left "Failed reading: replace filter requires find, replace strings as args"

case_filterCell8 = parseOnly filterCell "repalce : \'foo\', \'bar\' " @?=
  Left "Failed reading: \"repalce\": function isn't supported"

case_filterCell9 = parseOnly filterCell "toSentenceWithSeparator: \', \', \'and\' " @?=
  Right (FilterCell "toSentenceWithSeparator" [QuoteString ", ", QuoteString "and"])

case_filterCell10 = parseOnly filterCell "toSentenceWithSeparator: \', \' " @?=
  Left "Failed reading: toSentenceWithSeparator requires a separator argument and last element separator"

case_filterCell11 = parseOnly filterCell "renderWithSeparator: \', \' " @?=
  Right (FilterCell "renderWithSeparator" [QuoteString ", "])

--------------------------------------------------------------------------------
-- * Filter Cells
--------------------------------------------------------------------------------

case_filterCells1 = parseOnly filterCells "toUpper|toLower " @?=
  Right [FilterCell "toUpper" [], FilterCell "toLower" []]

case_filterCells2 = parseOnly filterCells "toUpper | replace: \'foo\', \'bar\' " @?=
  Right [FilterCell "toUpper" [], FilterCell "replace" [QuoteString "foo", QuoteString "bar"]]

case_filterCells3 = parseOnly filterCells "toUpper | replace: \'foo\', \'bar\' | toTitle " @?=
  Right [ FilterCell "toUpper" []
        , FilterCell "replace" [QuoteString "foo", QuoteString "bar"]
        , FilterCell "toTitle" []
        ]

case_filterCells4 = parseOnly filterCells "toUpper|replace:\'foo\',\'bar\'|toTitle " @?=
  Right [ FilterCell "toUpper" []
        , FilterCell "replace" [QuoteString "foo", QuoteString "bar"]
        , FilterCell "toTitle" []
        ]

case_filterCells5 = parseOnly filterCells "toUpper|replace:\'foo\',\'bar\'|toTitle | toUpper " @?=
  Right [ FilterCell "toUpper" []
        , FilterCell "replace" [QuoteString "foo", QuoteString "bar"]
        , FilterCell "toTitle" []
        , FilterCell "toUpper" []
        ]

--------------------------------------------------------------------------------
-- * Filter Block
--------------------------------------------------------------------------------

case_filterBlock1 = parseOnly filterBlock "\'a\' | toUpper|toLower " @?=
  Right (Filter (QuoteString "a") [FilterCell "toUpper" [], FilterCell "toLower" []])

case_filterBlock2 = parseOnly filterBlock " \"a\"|toUpper | replace: \'foo\', \'bar\' " @?=
  Right (Filter (QuoteString "a") [FilterCell "toUpper" [], FilterCell "replace" [QuoteString "foo", QuoteString "bar"]])

case_filterBlock3 = parseOnly filterBlock " abc |toUpper | replace: \'foo\', \'bar\' " @?=
  Right (Filter (Variable $ ObjectIndex "abc" :| [])
                [FilterCell "toUpper" [], FilterCell "replace" [QuoteString "foo", QuoteString "bar"]])

case_filterBlock4 = parseOnly filterBlock "abc[1].d|toUpper | replace: \'foo\', \'bar\' " @?=
  Right (Filter (Variable $ ObjectIndex "abc" :| [ArrayIndex 1, ObjectIndex "d"])
                [FilterCell "toUpper" [], FilterCell "replace" [QuoteString "foo", QuoteString "bar"]])

--------------------------------------------------------------------------------
-- * Output
--------------------------------------------------------------------------------

case_output1 = parseOnly output "{{ \'a\' | toUpper|toLower}}" @?=
  Right (Output (Filter (QuoteString "a") [FilterCell "toUpper" [], FilterCell "toLower" []]))

case_output2 = parseOnly output "{{\'a\' | toUpper|toLower}}" @?=
  Right (Output (Filter (QuoteString "a") [FilterCell "toUpper" [], FilterCell "toLower" []]))

case_output3 = parseOnly output "{{\'a\'| toUpper   | toLower}}" @?=
  Right (Output (Filter (QuoteString "a") [FilterCell "toUpper" [], FilterCell "toLower" []]))

case_output4 = parseOnly output "{{\'a\'| toUpper   | toLower }}" @?=
  Right (Output (Filter (QuoteString "a") [FilterCell "toUpper" [], FilterCell "toLower" []]))

case_output5 = parseOnly output "{{ abc |toUpper | replace: \'foo\', \'bar\' }}" @?=
  Right (Output (Filter (Variable $ ObjectIndex "abc" :| [])
                        [FilterCell "toUpper" [], FilterCell "replace" [QuoteString "foo", QuoteString "bar"]]))

case_output6 = parseOnly output "{{   abc[1].d  |toUpper |replace: \'foo\',\'bar\'}}" @?=
  Right (Output (Filter (Variable $ ObjectIndex "abc" :| [ArrayIndex 1, ObjectIndex "d"])
                        [FilterCell "toUpper" [], FilterCell "replace" [QuoteString "foo", QuoteString "bar"]]))

case_output7 = parseOnly output "{{\'a\'}}" @?= Right (Output $ QuoteString "a")
case_output8 = parseOnly output "{{ \'a\'}}" @?= Right (Output $ QuoteString "a")
case_output9 = parseOnly output "{{\'a\' }}" @?= Right (Output $ QuoteString "a")
case_output10 = parseOnly output "{{ \'a\' }}" @?= Right (Output $ QuoteString "a")
case_output11 = parseOnly output "{{ \'a\' }" @?= Left "Failed reading: Tag or output statement incomplete"
case_output12 = parseOnly output "{{ a\' }" @?= Left "Failed reading: Tag or output statement incomplete"
case_output13 = parseOnly output "{{a}}" @?= Right (Output $ Variable $ ObjectIndex "a" :| [])
case_output14 = parseOnly output "{{ a}}" @?= Right (Output $ Variable $ ObjectIndex "a" :| [])
case_output15 = parseOnly output "{{ a}}" @?= Right (Output $ Variable $ ObjectIndex "a" :| [])
case_output16 = parseOnly output "{{ a }}" @?= Right (Output $ Variable $ ObjectIndex "a" :| [])
case_output17 = parseOnly output "{{ a.b }}" @?= Right (Output $ Variable $ ObjectIndex "a" :| [ObjectIndex "b"])
case_output18 = parseOnly output "{{ a.b[12]}}" @?= Right (Output $ Variable $ ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 12])
case_output19 = parseOnly output "{{ a.b[12] }}" @?= Right (Output $ Variable $ ObjectIndex "a" :| [ObjectIndex "b", ArrayIndex 12])
case_output20 = parseOnly output "{{ a.b[12] }" @?= Left "Failed reading: Tag or output statement incomplete"

--------------------------------------------------------------------------------
-- * If logic
--------------------------------------------------------------------------------

case_ifLogic1 = parseOnly ifLogic "{% if 1 == a %} foo {% endif %}" @?=
  Right (IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                 (TrueStatements [(RawText " foo ")]))

case_ifLogic2 = parseOnly ifLogic "{% if 1 == a %} foo {% endif %}" @?=
  Right (IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                 (TrueStatements [(RawText " foo ")]))

case_ifLogic3 = parseOnly ifLogic "{% if 1 == a %}{{abc}}{% endif %}" @?=
  Right (IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                 (TrueStatements [Output $ (Variable $ ObjectIndex "abc" :| [])]))

case_ifLogic4 = parseOnly ifLogic "{% if 1 == a %}{{abc}} ok{% endif %}" @?=
  Right (IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                 (TrueStatements [(Output $ Variable $ ObjectIndex "abc" :| []), (RawText " ok")]))

case_ifLogic5 = parseOnly ifLogic "{% if 1 == a %}{{abc}} ok{% else %}yo{% endif %}" @?=
  Right (IfLogic (IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                          (TrueStatements [(Output $ Variable $ ObjectIndex "abc" :| []), (RawText " ok")]))
                          (IfLogic Else (TrueStatements [RawText "yo"])))

case_ifLogic6 = parseOnly ifLogic "{% if 1 == a %}{{abc}}{%elsif true%}ok{% else %}yo{% endif %}" @?=
  Right (IfLogic (IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                                    (TrueStatements [(Output $ Variable $ ObjectIndex "abc" :| [])]))
                          (IfLogic (IfLogic (ElsIfClause Trueth) (TrueStatements [RawText "ok"]))
                                   (IfLogic Else (TrueStatements [RawText "yo"]))))

--------------------------------------------------------------------------------
-- * Case logic
--------------------------------------------------------------------------------

case_caseLogic1 = parseOnly caseLogic "{% case a %}{% when 1 %}foo{% endcase %}" @?=
  Right (CaseLogic (Variable $ ObjectIndex "a" :| [])
                   [(Num 1, TrueStatements [RawText "foo"])])

case_caseLogic2 = parseOnly caseLogic "{% case a %}{% when 1 %}foo{{bar}}{% endcase %}" @?=
  Right (CaseLogic (Variable $ ObjectIndex "a" :| [])
                   [(Num 1, TrueStatements [RawText "foo", Output $ Variable $ ObjectIndex "bar" :| []])])

case_caseLogic3 = parseOnly caseLogic "{% case a %}{% when 1 %}foo{% when \'a\' %}baz{% endcase %}" @?=
  Right (CaseLogic (Variable $ ObjectIndex "a" :| [])
                   [(Num 1, TrueStatements [RawText "foo"])
                   ,(QuoteString "a", TrueStatements [RawText "baz"])
                   ])

case_caseLogic4 = parseOnly caseLogic "{% case a %}{% when 1 %}foo{% when \'a\' %}{% endcase %}" @?=
  Right (CaseLogic (Variable $ ObjectIndex "a" :| [])
                   [(Num 1, TrueStatements [RawText "foo"])
                   ,(QuoteString "a", TrueStatements [])
                   ])

case_caseLogic5 = parseOnly caseLogic "{% case a %}{% when 1 %}foo{% when \'a\' %}baz{% else %}quux{% endcase %}" @?=
  Right (CaseLogic (Variable $ ObjectIndex "a" :| [])
                   [(Num 1, TrueStatements [RawText "foo"])
                   ,(QuoteString "a", TrueStatements [RawText "baz"])
                   ,(Else, TrueStatements [RawText "quux"])
                   ])

--------------------------------------------------------------------------------
-- * Blocks
--------------------------------------------------------------------------------

case_block1 = parseOnly block "{% if 1 == a %}{{abc}} ok{% endif %}" @?=
  Right (IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                 (TrueStatements [(Output $ Variable $ ObjectIndex "abc" :| []), (RawText " ok")]))

case_block2 = parseOnly block "{{ abc}}" @?= Right (Output (Variable $ ObjectIndex "abc" :| []))
case_block3 = parseOnly block " abc" @?= Right (RawText " abc")

case_templateParser1 = parseOnly templateParser " abc" @?= Right [RawText " abc"]

case_templateParser2 = parseOnly templateParser " abc{{ abc}}" @?=
  Right [RawText " abc", Output (Variable $ ObjectIndex "abc" :| [])]

case_templateParser3 = parseOnly templateParser " abc{% if 1 == a %} foo {% endif %}{{ abc}}" @?=
  Right [ RawText " abc"
        , (IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                             (TrueStatements [(RawText " foo ")]))
        , Output (Variable $ ObjectIndex "abc" :| [])
        ]

case_templateParser4 = parseOnly templateParser " abc{% case a %}{% when 1 %}foo{% when \'a\' %}baz{% else %}quux{% endcase %}{% if 1 == a %} foo {% endif %}{{ abc}}" @?=
  Right [ RawText " abc"
        , (CaseLogic (Variable $ ObjectIndex "a" :| [])
                   [(Num 1, TrueStatements [RawText "foo"])
                   ,(QuoteString "a", TrueStatements [RawText "baz"])
                   ,(Else, TrueStatements [RawText "quux"])
                   ])
        , (IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                             (TrueStatements [(RawText " foo ")]))
        , Output (Variable $ ObjectIndex "abc" :| [])
        ]

case_templateParser5 = parseOnly templateParser " abc{% case a %}{% when 1 %}foo{% when \'a\' %}baz{% else %}quux{% endcase %}{% if 1 == a %} foo {% endif %}{{ abc}}{% raw %}what{}%{% endraw %}" @?=
  Right [ RawText " abc"
        , (CaseLogic (Variable $ ObjectIndex "a" :| [])
                   [(Num 1, TrueStatements [RawText "foo"])
                   ,(QuoteString "a", TrueStatements [RawText "baz"])
                   ,(Else, TrueStatements [RawText "quux"])
                   ])
        , (IfLogic (IfClause (Equal (Num $ sc 1) (Variable $ ObjectIndex "a" :| [])))
                             (TrueStatements [(RawText " foo ")]))
        , Output (Variable $ ObjectIndex "abc" :| [])
        , RawText "what{}%"
        ]

case_templateParser6 = parseOnly templateParser "" @?= Left "Syntax Error > Block Parsing: Failed reading: empty"

case_templateParser7 = parseOnly templateParser "{% assign a = 1 %}{{ a }}" @?=
  Right [ AssignClause (Variable $ ObjectIndex "a" :| []) (Num 1)
        , Output (Variable (ObjectIndex "a" :| []))
        ]

prop_templateP_is_lawful1 =
  forAll genTemplateExpr (\t -> (preview templateP (review templateP t)) == Just t)

-- | n.b. Must remain at the bottom of the file for TH
parserTests :: TestTree
parserTests = $(testGroupGenerator)

