{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Liquid.Tokens (
    and
  , assign
  , break
  , cBr
  , cPar
  , captureEnd
  , captureStart
  , caseEnd
  , caseStart
  , colon
  , comma
  , commentEnd
  , commentStart
  , contains
  , continue
  , cycle
  , dot
  , dotDot
  , els
  , elsIf
  , empty
  , end
  , endIf
  , eq
  , eqSign
  , false
  , forEnd
  , forStart
  , gt
  , gtEq
  , iN
  , ifKeyStart
  , ifStart
  , include
  , lt
  , ltEq
  , minus
  , neq
  , nil
  , null
  , oBr
  , oPar
  , or
  , outputEnd
  , outputStart
  , pipe
  , qMark
  , rawEnd
  , rawStart
  , tableEnd
  , tableStart
  , tagEnd
  , tagStart
  , true
  , underscore
  , unlessEnd
  , unlessStart
  , when
  , with
  ) where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text
import           Data.Text            (Text)
import           Prelude              (Char)

--------------------------------------------------------------------------------
-- * Symbolic Tokens
--------------------------------------------------------------------------------

colon :: Parser Char
colon = char ':'

comma :: Parser Char
comma = char ','

dot :: Parser Char
dot = char '.'

dotDot :: Parser Text
dotDot = ".."

eq :: Parser Text
eq = "=="

eqSign :: Parser Char
eqSign = char '='

gt :: Parser Char
gt = char '>'

gtEq :: Parser Text
gtEq = ">="

lt :: Parser Char
lt = char '<'

ltEq :: Parser Text
ltEq = "<="

minus :: Parser Char
minus = char '-'

neq :: Parser Text
neq = "!=" <|> "<>"

oBr, cBr :: Parser Char
oBr = char '['
cBr = char ']'

oPar, cPar :: Parser Char
oPar = char '('
cPar = char ')'

outputStart, outputEnd :: Parser Text
outputStart = "{{"
outputEnd = "}}"

pipe :: Parser Char
pipe = char '|'

qMark :: Parser Char
qMark = char '?'

tagStart, tagEnd :: Parser Text
tagStart = "{%"
tagEnd = "%}"

underscore :: Parser Char
underscore = char '_'

--------------------------------------------------------------------------------
-- * Textual Tokens
--------------------------------------------------------------------------------

and :: Parser Text
and = "and"

assign :: Parser Text
assign = "assign"

break, continue :: Parser Text
break = "break"
continue = "continue"

captureStart, captureEnd :: Parser Text
captureStart = "capture"
captureEnd = "endcapture"

caseStart, caseEnd :: Parser Text
caseStart = "case"
caseEnd = "endcase"

commentStart, commentEnd :: Parser Text
commentStart = "comment"
commentEnd = "endcomment"

contains :: Parser Text
contains = "contains"

cycle :: Parser Text
cycle = "cycle"

els :: Parser Text
els = "else"

elsIf :: Parser Text
elsIf = "elsif"

empty :: Parser Text
empty = "empty"
end :: Parser Text
end = "end"

endIf :: Parser Text
endIf = "endif"

forStart, forEnd :: Parser Text
forStart = "for"
forEnd = "endfor"

iN :: Parser Text
iN = "in"

ifKeyStart :: Parser Text
ifKeyStart = "ifkey"

ifStart :: Parser Text
ifStart = "if"

include :: Parser Text
include = "include"

null, nil :: Parser Text
null = "null"
nil = "nil"

or :: Parser Text
or = "or"

rawStart, rawEnd :: Parser Text
rawStart = "raw"
rawEnd = "endraw"

tableStart, tableEnd :: Parser Text
tableStart = "tablerow"
tableEnd = "endtablerow"

true, false :: Parser Text
true = "true"
false = "false"

unlessStart, unlessEnd :: Parser Text
unlessStart = "unless"
unlessEnd = "endunless"

when :: Parser Text
when = "when"

with :: Parser Text
with = "with"

