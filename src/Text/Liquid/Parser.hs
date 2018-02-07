{-# LANGUAGE OverloadedStrings #-}

module Text.Liquid.Parser where

import           Prelude                    hiding (and, null, or, takeWhile)

import           Control.Applicative
import           Control.Lens               (Prism', prism')
import           Data.Attoparsec.Combinator (lookAhead)
import           Data.Attoparsec.Text
import           Data.Char                  (isAlpha)
import           Data.List.NonEmpty         (NonEmpty (..), nonEmpty)
import           Data.Scientific            (toBoundedInteger)
import           Data.Semigroup             hiding (option)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Liquid.Helpers
import           Text.Liquid.Tokens
import           Text.Liquid.Types


-- | Match middle parser, around explicit start and end parsers
between
  :: Parser b -- ^ open tag parser
  -> Parser b -- ^ close tag parser
  -> Parser a -- ^ match middle parser
  -> Parser a
between open close p = do
  _ <- open
  x <- p
  (close *> return x) <|> fail "Tag or output statement incomplete"

-- | Match parser between whitespace
stripped
  :: Parser a
  -> Parser a
stripped =
  between skipSpace skipSpace

-- | Match given parser for a tag
tag
  :: Parser a
  -> Parser a
tag p =
  between tagStart tagEnd (stripped p)

-- | Match given parser for output block
outputTag
  :: Parser a
  -> Parser a
outputTag p =
  between outputStart outputEnd (stripped p)

-- | Match given tag name (e.g. for, case) with following parser
tagWith
  :: Parser a -- ^ initial tag type, e.g. for
  -> Parser b -- ^ follow on parser, e.g. variable
  -> Parser b
tagWith tg p =
  tag $ tg *> skipSpace >> p

-- | Convert match into text
mapT
  :: Parser [Char]
  -> Parser Text
mapT =
  fmap T.pack

-- | Match variables (without indices, including underscore or hash)
var :: Parser Text
var = mapT $ many1 $ letter <|> satisfy (inClass "_-")

-- | Parse a positive integer within square brackets, e.g. "[123]", NOT "[123.1]"
parseBoxedInt :: Parser Int
parseBoxedInt = do
    sc <- between oBr cBr scientific
    case toBoundedInteger sc of
      Just i  -> if i >= 0 then return i else err
      Nothing -> err
  where err = fail "invalid variable (array) index, expecting a positive integer"

-- | Parse a variable section with an optional indexing
--   An array index MUST be preceded by an object index
--   ...hence Maybe do comprehension
varIndexSection :: Parser (NonEmpty VarIndex)
varIndexSection = do
  vs <- sepBy var dot
  i  <- many parseBoxedInt
  brokenChar <- oBr <|> return '~'
  let ixs = do obs <- (nonEmpty (ObjectIndex <$> vs))
               Just obs <> (nonEmpty (ArrayIndex <$> i))
  if brokenChar == '[' then (fail "invalid array index - ill-typed") else case ixs of
    Just nel -> return nel
    Nothing  -> fail "invalid var index section"

-- | Parse a variable
variable :: Parser Expr
variable = do
  sections <- sepBy1 varIndexSection dot
  return . Variable $ foldl1 (<>) sections

-- | e.g. raw tag, comment tag
rawBodyTag :: Parser Text -- ^ start tag matcher
           -> Parser Text -- ^ end tag matcher
           -> Parser Text
rawBodyTag s e =
  s >> skipSpace *> (mapT $ manyTill anyChar (skipSpace >> e))

-- | Match interior of raw tag
rawTag :: Parser Expr
rawTag = RawText <$> rawBodyTag (tag rawStart) (tag rawEnd)

-- | Match interior of comment tag
commentTag :: Parser Expr
commentTag = rawBodyTag (tag commentStart) (tag commentEnd) *> pure Noop

-- | Match any raw text upto a tag/output start or the end of the input
textPart :: Parser Expr
textPart = RawText <$> (mapT $ manyTill1 (satisfy $ notInClass "{%") terminator)
  where terminator = lookAhead $ tagStart <|> outputStart <|> (endOfInput *> pure T.empty)

-- | Force the first character to be valid, otherwise fail miserably
manyTill1 :: Alternative f => f a -> f b -> f [a]
manyTill1 p e = (:) <$> p <*> s
    where s = (e *> pure []) <|> ((:) <$> p <*> s)

-- | Match an Ord comparison operator
ordOperator :: Parser (Expr -> Expr -> Expr)
ordOperator =
  stripped $ eq *> pure Equal              <|>
             neq *> pure NotEqual          <|>
             gtEq *> pure GtEqual          <|>
             ltEq *> pure LtEqual          <|>
             (mapT . some $ gt) *> pure Gt <|>
             (mapT . some $ lt) *> pure Lt <|>
             contains *> pure Contains

-- | Match an or, and or contains predicate
ordCombinator :: Parser (Expr -> Expr -> Expr)
ordCombinator =
  stripped $ or  *> pure Or  <|>
             and *> pure And

-- | Match a quoted string
quoteString :: Parser Expr
quoteString = do
  skipSpace
  beginTick <- satisfy (inClass "\'\"")
  qs        <- mapT $ manyTill anyChar (char beginTick)
  return $ QuoteString qs

-- | Match a expression which is evaluated to value
valueExpression :: Parser Expr
valueExpression =
  immediateValue <|>
  variable       <|>
  predicate      <|>
  binaryPredicate

-- | Match a immediate value
immediateValue :: Parser Expr
immediateValue =
  quoteString                 <|>
  (Num <$> scientific)        <|>
  Null <$ (stripped null)     <|>
  Nil <$ (stripped nil)       <|>
  Falseth <$ (stripped false) <|>
  Trueth  <$ (stripped true)

-- | Match a binary predicate, e.g. a.b >= b.name
binaryPredicate :: Parser Expr
binaryPredicate = do
  lhs <- immediateValue <|>
         variable
  op  <- ordOperator
  rhs <- immediateValue <|>
         variable
  return $ op lhs rhs

-- | Parse and evaluate truthiness
truthy :: Parser Expr
truthy =
  Null    <$ (stripped null)              <|>
  Nil     <$ (stripped nil)               <|>
  Falseth <$ (stripped false)             <|>
  Trueth  <$ (stripped true)              <|>
  (Truthy . Num <$> stripped scientific)  <|>
  (Truthy       <$> stripped quoteString) <|>
  (Truthy       <$> stripped variable)

-- | Match a binary predicate, e.g. a.b >= b.name or 'barry'
predicate :: Parser Expr
predicate = do
  bpl <- stripped binaryPredicate
  oc  <- ordCombinator
  bpr <- stripped binaryPredicate
  return $ oc bpl bpr

-- | Match any predicate clause
predicateClause :: Parser Expr
predicateClause =
  predicate <|> binaryPredicate <|> truthy

-- | Match an if clause
ifClause :: Parser Expr
ifClause = IfClause <$> tagWith ifStart predicateClause

-- | Match an ifkey clause
ifKeyClause :: Parser Expr
ifKeyClause = IfKeyClause <$> tagWith ifKeyStart variableOnly
  where variableOnly = do res <- eitherP precheck variable
                          case res of
                            Left _   -> fail "Only variables as ifkey args allowed"
                            Right ok -> return ok
        precheck     = Null    <$ (stripped null)             <|>
                       Nil     <$ (stripped nil)              <|>
                       Falseth <$ (stripped false)            <|>
                       Trueth  <$ (stripped true)             <|>
                       (Truthy . Num <$> stripped scientific) <|>
                       (Truthy       <$> stripped quoteString)

-- | Match an elsif clause
elsifClause :: Parser Expr
elsifClause = ElsIfClause <$> tagWith elsIf predicateClause

-- | Match an else clause
elseClause :: Parser Expr
elseClause = (tag els) *> pure Else

-- | Match the end of an if clause
endIfClause :: Parser Expr
endIfClause = (tag endIf) *> pure Noop

-- | Match a variable condition for a case clause
caseClause :: Parser Expr
caseClause = tagWith caseStart variable

-- | Match a when clause, part of a case pattern match block
whenClause :: Parser Expr
whenClause = tagWith when (quoteString <|> (Num <$> scientific))

-- | Match the end of a case pattern match block
endCaseClause :: Parser Expr
endCaseClause = (tag caseEnd) *> pure Noop

-- | Match an assign clause
assignClause :: Parser Expr
assignClause = do
  (var, val) <- tagWith assign assignExpr
  pure $ AssignClause var val
  where
    assignExpr :: Parser (Expr, Expr)
    assignExpr = do
      var <- variable
      _ <- skipSpace *> eqSign
      val <- skipSpace *> valueExpression
      pure (var, val)

-- | Match a for clause
forClause :: Parser Expr
forClause = do
  (elmVar, arrVar) <- tagWith forStart inClause
  return $ ForClause elmVar arrVar
  where
    inClause :: Parser (Expr, Expr)
    inClause = do
      elmVar <- skipSpace *> variable
      _ <- skipSpace *> iN
      arrVar <- skipSpace *> variable
      return (elmVar, arrVar)

-- | Match the end of an for clause
endForClause :: Parser Expr
endForClause = (tag forEnd) *> pure Noop

-- | Match a filter fn name
filterName :: Parser Text
filterName = mapT $ skipSpace *> manyTill1 letter terminator
  where terminator = colon              <|>
                    (skipSpace *> pipe) <|>
                    (lookAhead $ satisfy (not . isAlpha))

-- | Match the list of arguments for the filter fn
filterArgs :: Parser [Expr]
filterArgs = skipSpace *> sepBy numOrString comma
  where numOrString = skipSpace *> (Num <$> scientific) <|> quoteString

-- | Match a filter cell, fn and args
filterCell :: Parser Expr
filterCell = do
  fnName <- filterName
  args   <- filterArgs
  typeCheckFilter fnName args

-- | Type check the function args and check arity
typeCheckFilter :: Text
                -> [Expr]
                -> Parser Expr
typeCheckFilter "toUpper"             []                                   =
  return $ FilterCell "toUpper" []
typeCheckFilter "toUpper"             _                                    =
  fail "toUpper filter takes no arguments"
typeCheckFilter "toLower"             []                                   =
  return $ FilterCell "toLower" []
typeCheckFilter "toLower"             _                                    =
  fail "toLower filter takes no arguments"
typeCheckFilter "toTitle"             []                                   =
  return $ FilterCell "toTitle" []
typeCheckFilter "toTitle"             _                                    =
  fail "toTitle filter takes no arguments"
typeCheckFilter "replace"             a@[QuoteString _, QuoteString _]     =
  return $ FilterCell "replace"       a
typeCheckFilter "replace"             _                                    =
  fail "replace filter requires find, replace strings as args"
typeCheckFilter "first"               []                                   =
  return $ FilterCell "first" []
typeCheckFilter "first"               _                                    =
  fail "first filter takes no arguments"
typeCheckFilter "firstOrDefault"      a@(_:_)                              =
  return $ FilterCell "firstOrDefault" a
typeCheckFilter "firstOrDefault" _                                         =
  fail "firstOrDefault requires a single default parameter"
typeCheckFilter "last"                []                                   =
  return $ FilterCell "last" []
typeCheckFilter "last"           _                                         =
  fail "last filter takes no arguments"
typeCheckFilter "lastOrDefault"       a@(_:_)                              =
  return $ FilterCell "lastOrDefault" a
typeCheckFilter "lastOrDefault"       _                                    =
  fail "lastOrDefault requires a single default parameter"
typeCheckFilter "countElements"       []                                   =
  return $ FilterCell "countElements" []
typeCheckFilter "countElements"       _                                    =
  fail "countElements takes no arguments"
typeCheckFilter "renderWithSeparator" a@[QuoteString _]                    =
  return $ FilterCell "renderWithSeparator" a
typeCheckFilter "renderWithSeparator" _                                    =
  fail "renderWithSeparator requires a separator argument with which to intersperse the target array"
typeCheckFilter "toSentenceWithSeparator" a@[QuoteString _, QuoteString _] =
  return $ FilterCell "toSentenceWithSeparator" a
typeCheckFilter "toSentenceWithSeparator" _                                =
  fail "toSentenceWithSeparator requires a separator argument and last element separator"
typeCheckFilter l                     _                                    =
  fail $ (show l) ++ ": function isn't supported"

-- | Match multiple filter fns and args
filterCells :: Parser [Expr]
filterCells = many ((filterCell <* (skipSpace *> pipe)) <|> filterCell)

-- | Match a lhs and a block of filters with their args
filterBlock :: Parser Expr
filterBlock = do
  lhs   <- (quoteString <|> (skipSpace *> variable)) <* (skipSpace >> pipe)
  cells <- filterCells
  return $ Filter lhs cells

-- | Output block, a variable, indexed variable, number or filter block
output :: Parser Expr
output = Output <$> outputTag (filterBlock <|> quoteString <|> variable)

-- | If statement, optional elsif or else
ifLogic :: Parser Expr
ifLogic = do
  start  <- ifClause <|> ifKeyClause <|> elsifClause <|> elseClause
  iftrue <- TrueStatements <$>
            manyTill (output <|> textPart)
                     (lookAhead elsifClause <|>
                      lookAhead elseClause  <|>
                      lookAhead endIfClause)
  let sofar = IfLogic start iftrue
  (endIfClause *> pure sofar) <|> (IfLogic sofar <$> ifLogic)

-- | Case pattern match block
caseLogic :: Parser Expr
caseLogic = do
    start    <- caseClause
    patterns <- many1 whenBlock
    _        <- endCaseClause
    return $ CaseLogic start patterns
  where whenBlock = do
          pattern <- whenClause <|> elseClause
          iftrue  <- TrueStatements <$>
                     manyTill (output <|> textPart)
                              (lookAhead whenClause <|>
                               lookAhead elseClause <|>
                               lookAhead endCaseClause)
          return (pattern, iftrue)

-- | For iteration
forLogic :: Parser Expr
forLogic = do
  start <- forClause
  iteration <-
    TrueStatements <$>
      manyTill (output <|> textPart) endForClause
  return $ ForLogic start iteration

-- | Parse any block type
block :: Parser Expr
block = choice [ ifLogic
               , caseLogic
               , assignClause
               , rawTag
               , commentTag
               , output
               , textPart
               ] <?> "Block Parsing"

-- | Parse an entire template into chunks
templateParser :: Parser [Expr]
templateParser = manyTill1 (block <?> "Syntax Error") endOfInput

-- | Run the templateParser on input text, force partial results to terminate with Failure
parseTemplate :: Text
              -> IResult Text [Expr]
parseTemplate t =
  feed (parse templateParser t) T.empty

templateP :: Prism' Text [Expr]
templateP = prism' back forw
  where forw = maybeResult . parseTemplate
        back = mconcat . fmap renderExpr

