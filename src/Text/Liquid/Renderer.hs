{-# LANGUAGE OverloadedStrings #-}

module Text.Liquid.Renderer where

import           Control.Applicative
import           Control.Lens         hiding (op, (<|))
import           Data.Aeson           hiding (Null)
import           Data.Aeson.Lens
import qualified Data.Aeson.Lens      as AL
import           Data.Attoparsec.Text
import           Data.Bifunctor       (second)
import           Data.Foldable        (foldl')
import           Data.List            (intersperse)
import           Data.List.NonEmpty   (NonEmpty (..), (<|))
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          ((<>))
import           Data.Scientific      (Scientific)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Validation
import           Text.Liquid.Helpers
import           Text.Liquid.Parser   (parseTemplate)
import           Text.Liquid.Types


-- | Interpret function - for use in testing the lib
interpretWithJson
  :: Value -- ^ JSON context
  -> Text  -- ^ Raw template
  -> Rendering Text
interpretWithJson ctx template = case parseRes of
    (Fail brokenPart errCtxs _) ->
      let (ok, bad) = T.breakOn brokenPart template
      in _Failure # [ TemplateParsingError ok bad (T.pack <$> errCtxs) ]
    (Partial _)    -> _Failure # [ LiquidError "Major parsing error! - Attoparsec Issue" ]
    (Done _ ts)  -> interpret ctx ts
  where parseRes = parseTemplate template

-- | Interpret the raw data if it is ok
interpret
  :: Value
  -> [Expr]
  -> Rendering Text
interpret j es = fst <$> renderTemplates j es

-- | Render list of expression
renderTemplates :: Value -> [Expr] -> Rendering (Text, Value)
renderTemplates j =
  foldl' go (pure (T.empty, j))
  where
    go :: Rendering (Text, Value) -> Expr -> Rendering (Text, Value)
    go (AccSuccess (x, j')) t = (\(x', j'') -> (x <> x', j'')) <$> renderTemplate j' t
    go failure _ = failure

-- | Main template block rendering fn
renderTemplate
  :: Value
  -> Expr
  -> Rendering (Text, Value)

-- | Rendering types
renderTemplate j (Output f@(Filter _ _))      = idContext j (flip applyFilter f)
renderTemplate j (Output q@(QuoteString _))   = idContext j (flip renderText q)
renderTemplate j (Output v@(Variable _))      = idContext j (flip renderText v)
renderTemplate j (Output n@(Num _))           = idContext j (flip renderText n)
renderTemplate j (RawText t)                  = idContext j (const $ pure t)

-- | If logic
renderTemplate j (IfLogic (IfClause i)
                          (TrueStatements ts)) =
  evalLogic j (evalTruthiness j i) ts

renderTemplate j (IfLogic (IfLogic (IfClause it) (TrueStatements ts))
                          (IfLogic Else (TrueStatements ets)))
    | ifTrue == (AccSuccess False) = evalLogic j (pure True) ets
    | otherwise                    = evalLogic j ifTrue ts
  where ifTrue = evalTruthiness j it

renderTemplate j (IfLogic (IfLogic (IfClause it) (TrueStatements ts))
                          (IfLogic (ElsIfClause eit) (TrueStatements eits)))
    | ifTrue == (AccSuccess False) = evalLogic j ifElseTrue eits
    | otherwise                    = evalLogic j ifTrue ts
  where ifTrue     = evalTruthiness j it
        ifElseTrue = evalTruthiness j eit

renderTemplate j (IfLogic (IfLogic (IfClause it) (TrueStatements ts))
                          (IfLogic (IfLogic (ElsIfClause eit) (TrueStatements eits))
                                   (IfLogic Else (TrueStatements ets))))
    | ifTrue == (AccSuccess False) && ifElseTrue == (AccSuccess False) = evalLogic j (pure True) ets
    | ifTrue == (AccSuccess False)                                     = evalLogic j ifElseTrue eits
    | otherwise                    = evalLogic j ifTrue ts
    where ifTrue     = evalTruthiness j it
          ifElseTrue = evalTruthiness j eit

-- | Ifkey logic
renderTemplate j (IfLogic (IfKeyClause i)
                          (TrueStatements ts)) =
  evalLogic j (evalKeyTruthiness j i) ts

renderTemplate j (IfLogic (IfLogic (IfKeyClause it) (TrueStatements ts))
                          (IfLogic Else (TrueStatements ets)))
    | ifTrue == (AccSuccess False) = evalLogic j (pure True) ets
    | otherwise                    = evalLogic j ifTrue ts
  where ifTrue = evalKeyTruthiness j it

renderTemplate j (IfLogic (IfLogic (IfKeyClause it) (TrueStatements ts))
                          (IfLogic (ElsIfClause eit) (TrueStatements eits)))
    | ifTrue == (AccSuccess False) = evalLogic j ifElseTrue eits
    | otherwise                    = evalLogic j ifTrue ts
  where ifTrue     = evalKeyTruthiness j it
        ifElseTrue = evalTruthiness j eit

renderTemplate j (IfLogic (IfLogic (IfKeyClause it) (TrueStatements ts))
                          (IfLogic (IfLogic (ElsIfClause eit) (TrueStatements eits))
                                   (IfLogic Else (TrueStatements ets))))
    | ifTrue == (AccSuccess False) && ifElseTrue == (AccSuccess False) = evalLogic j (pure True) ets
    | ifTrue == (AccSuccess False)                                     = evalLogic j ifElseTrue eits
    | otherwise                    = evalLogic j ifTrue ts
    where ifTrue     = evalKeyTruthiness j it
          ifElseTrue = evalTruthiness j eit

-- | Case logic
renderTemplate j (CaseLogic (Variable v) patterns) =
  evalCaseLogic j (extractValue j v) patterns

-- | Assign
renderTemplate j (AssignClause (Variable var) t) =
  case evalValue j t of
    AccSuccess v ->
      case attachContext var v j of
        Right j' -> pure (T.empty, j')
        Left err -> _Failure # [ RenderingFailure err ]
    failure -> idContext j (const $ second (const T.empty) failure)

-- | For logic
renderTemplate j (ForLogic (ForClause (Variable x) xs) (TrueStatements ts)) =
  case evalValue j xs of
    AccSuccess v -> evalForLogic j x v ts
    failure -> idContext j $ const $ second (const T.empty) failure

-- | Catch all error - theoretically impossible.
renderTemplate _ _ =
  _Failure # [ LiquidError "Template rendering critical error!" ]

-- | Evaluate predicate result and render
evalLogic
  :: Value          -- ^ JSON Context
  -> Rendering Bool -- ^ Predicate / logical expression result
  -> [Expr]         -- ^ Expressions to evaluate if true
  -> Rendering (Text, Value)
evalLogic j (AccSuccess True) ts = renderTemplates j ts
evalLogic j (AccSuccess False) _ = idContext j (const $ pure T.empty)
evalLogic j failure _            = idContext j (const $ second (const T.empty) failure)

-- | Evaluate case logic
evalCaseLogic
  :: Value
  -> Rendering Value -- ^ Extracted JSON value for case match
  -> [(Expr, Expr)]
  -> Rendering (Text, Value)
evalCaseLogic j _ []                                      =
  pure (T.empty, j)
evalCaseLogic j v ((Num n, TrueStatements ts):xs)         =
  case (==) (pure n) <$> preview _Number <$> v of
    AccSuccess True  -> evalLogic j (pure True) ts
    AccSuccess False -> evalCaseLogic j v xs
    failure          -> idContext j (const $ second (const T.empty) failure)
evalCaseLogic j v ((QuoteString q, TrueStatements ts):xs) =
  case (==) (pure q) <$> preview _String <$> v of
    AccSuccess True  -> evalLogic j (pure True) ts
    AccSuccess False -> evalCaseLogic j v xs
    failure          -> idContext j (const $ second (const T.empty) failure)
evalCaseLogic j _ ((Else, TrueStatements ts):[])          =
  evalLogic j (pure True) ts
evalCaseLogic _ v ((Else, TrueStatements _):_)            =
  _Failure # [ RenderingFailure "Multiple else blocks in a case statement" ] <*> v
evalCaseLogic _ v _                                       =
  _Failure # [ RenderingFailure "Impossible case pattern evaluation" ] <*> v

-- | Evaluate for logic
evalForLogic :: Value -> JsonVarPath -> Value -> [Expr] -> Rendering (Text, Value)
evalForLogic j p (Array a) ts =
  foldl' go (pure (T.empty, j)) a
  where
    go :: Rendering (Text, Value) -> Value -> Rendering (Text, Value)
    go (AccSuccess (t, j')) v =
      case attachContext p v j' of
        Right j'' -> (\(t', j''') -> (t <> t', j''')) <$> renderTemplates j'' ts
        Left err -> _Failure # [ RenderingFailure err ]
    go failure _ = failure
evalForLogic _ _ _ _ = _Failure # [ RenderingFailure "It cannot iterate other than array."]

-- | Render a renderable Expr as Text
renderText
  :: Value
  -> Expr
  -> Rendering Text
renderText _ Noop            = pure $ T.empty
renderText _ (RawText t)     = pure t
renderText _ (Num n)         = pure $ formatNum n
renderText j (Variable xs)   =
  numberOrTextFormat $ extractValue j xs
renderText _ (QuoteString q) = pure q
renderText _ e               =
  _Failure # [ RenderingFailure $ "Can't render this type: " <> (renderExpr e) ]

-- | Format variable as either number or Text
numberOrTextFormat
  :: Rendering Value
  -> Rendering Text
numberOrTextFormat rv =
    fromMaybe (AccFailure [err]) (s <|> n)
  where s = sequenceA $ preview _String <$> rv
        n = sequenceA $ (fmap formatNum <$> preview _Number <$> rv)
        err = NotAStringOrNumberJsonValue rv --TODO build better error

-- | Eval key present & not null
evalKeyTruthiness
  :: Value
  -> Expr
  -> Rendering Bool
evalKeyTruthiness j (Variable i@(ObjectIndex "user" :| _)) =
  maybe (pure False) (const (pure True)) (j ^? buildLens i.nonNull)
evalKeyTruthiness j (Variable i@(ObjectIndex "event" :| _)) =
  maybe (pure False) (const (pure True)) (j ^? buildLens i.nonNull)
evalKeyTruthiness j (Variable i) =
  maybe (pure False) (const (pure True)) $ (j ^? buildLens i.nonNull) <|> (j ^? buildLens (ObjectIndex "event" <| i).nonNull)
evalKeyTruthiness _ _            =
  _Failure # [ RenderingFailure "Can't evalulate if key on anything other than json context variables" ]

-- | Eval truth
evalTruthiness
  :: Value
  -> Expr
  -> Rendering Bool
evalTruthiness j (Truthy (Variable i))                     =
  case extractValue j i of
    (AccSuccess v) -> maybe (pure True) pure (v ^? _Bool)
    failure        -> second (const False) failure
evalTruthiness _ (Truthy _)                                = pure True
evalTruthiness _ Nil                                       = pure False
evalTruthiness _ Null                                      = pure False
evalTruthiness _ Falseth                                   = pure False
evalTruthiness _ Trueth                                    = pure True
evalTruthiness j (Equal l r)                               = bothSidesEqual j l r
evalTruthiness j (NotEqual l r)                            = not <$> bothSidesEqual j l r
evalTruthiness _ (GtEqual (Num l) (Num r))                 = pure $ l >= r
evalTruthiness _ (LtEqual (Num l) (Num r))                 = pure $ l <= r
evalTruthiness _ (Gt (Num l) (Num r))                      = pure $ l > r
evalTruthiness _ (Lt (Num l) (Num r))                      = pure $ l < r
evalTruthiness _ (GtEqual (QuoteString l) (QuoteString r)) = pure $ l >= r
evalTruthiness _ (LtEqual (QuoteString l) (QuoteString r)) = pure $ l <= r
evalTruthiness _ (Gt (QuoteString l) (QuoteString r))      = pure $ l > r
evalTruthiness _ (Lt (QuoteString l) (QuoteString r))      = pure $ l < r
evalTruthiness j (GtEqual l r)                             = varComparisons j (>=) l r
evalTruthiness j (LtEqual l r)                             = varComparisons j (<=) l r
evalTruthiness j (Gt l r)                                  = varComparisons j (>) l r
evalTruthiness j (Lt l r)                                  = varComparisons j (<) l r
evalTruthiness j (Contains l r)                            = containsCheck j l r
evalTruthiness j (Or l r)                                  =
  (||) <$> evalTruthiness j l <*> evalTruthiness j r
evalTruthiness j (And l r)                                 =
  (&&) <$> evalTruthiness j l <*> evalTruthiness j r
evalTruthiness _ err                                       =
  _Failure # [ ImpossibleTruthEvaluation err ]

-- | Check if the variable contains the thing on the rhs
containsCheck
  :: Value
  -> Expr
  -> Expr
  -> Rendering Bool
containsCheck j (Variable l) (QuoteString r) = elem r <$> v
  where v = getStringsFromArray <$> extractValue j l
containsCheck j (Variable l) (Num r)         = elem r <$> v
  where v = getNumbersFromArray <$> extractValue j l
containsCheck _ (Variable _) r               =
  _Failure # [ ImpossibleComparison "Contains" (renderExpr r) ]
containsCheck _ _ _                          =
  _Failure # [ LiquidError "Contains checks can only be performed on arrays (i.e. Variables)" ]

-- | Aggregate all the strings in the underlying array - if present
getStringsFromArray
  :: Value
  -> [Text]
getStringsFromArray v =
  v ^.. values . _String

-- | Aggregate all the numbers in the underlying array - if present
getNumbersFromArray
  :: Value
  -> [Scientific]
getNumbersFromArray v =
  v ^.. values . _Number

-- | Truth evaluation with variables
--   ONLY numberic comparison allowed, text comparisons not supported
varComparisons
  :: Value
  -> (Maybe Scientific -> Maybe Scientific -> Bool)
  -> Expr
  -> Expr
  -> Rendering Bool
varComparisons j op (Num l)      (Variable r)         = op (pure l) <$> vr
  where vr = preview _Number <$> extractValue j r
varComparisons j op (Variable l) (Num r)              = op <$> vl <*> (pure $ pure r)
  where vl = preview _Number <$> extractValue j l
varComparisons j op lhs@(Variable l) rhs@(Variable r) = res
  where vl = preview _Number <$> extractValue j l
        vr = preview _Number <$> extractValue j r
        res = case (vl, vr) of
                (AccSuccess (Just _), AccSuccess (Just _)) -> op <$> vl <*> vr
                (AccSuccess Nothing, AccSuccess (Just _))  ->
                  _Failure # [ ImpossibleComparison ("Number not found at variable" <> renderExpr lhs) (renderExpr rhs) ]
                (AccSuccess (Just _), AccSuccess Nothing)  ->
                  _Failure # [ ImpossibleComparison (renderExpr lhs) ("Number not found at variable" <> renderExpr rhs)]
                (_, _)                                     ->
                  _Failure # [ ImpossibleComparison ("Number not found at variable" <> renderExpr lhs)
                                                    ("Number not found at variable" <> renderExpr rhs) ]
varComparisons _ _  l            r            =
  _Failure # [ ImpossibleComparison (renderExpr l) (renderExpr r) ]

-- | Evaluate a binary comparison
bothSidesEqual
  :: Value -- ^ JSON context
  -> Expr  -- ^ lhs
  -> Expr  -- ^ rhs
  -> Rendering Bool
bothSidesEqual _ l r | l == r                      = pure True
bothSidesEqual _ (QuoteString q1) (QuoteString q2) = pure $ q1 == q2
bothSidesEqual j (Variable xs) (QuoteString q)     = (==) (pure q) <$> vl
  where vl = preview _String <$> extractValue j xs
bothSidesEqual j (QuoteString q) (Variable ys)     = (==) (pure q) <$> vr
  where vr = preview _String <$> extractValue j ys
bothSidesEqual j (Variable xs) (Variable ys)       = (==) <$> vl <*> vr
  where vl = extractValue j xs
        vr = extractValue j ys
bothSidesEqual j (Variable xs) (Num n)             = (==) (pure n) <$> vl
  where vl = preview _Number <$> extractValue j xs
bothSidesEqual j (Num n) (Variable ys)             = (==) (pure n) <$> vr
  where vr = preview _Number <$> extractValue j ys
bothSidesEqual _ (Num l) (Num r)                   = pure $ l == r
bothSidesEqual j (Variable xs) Trueth              = (==) (pure True) <$> vl
  where vl = preview _Bool <$> extractValue j xs
bothSidesEqual j Trueth (Variable xs)              = (==) (pure True) <$> vl
  where vl = preview _Bool <$> extractValue j xs
bothSidesEqual j (Variable xs) Falseth             = (==) (pure False) <$> vl
  where vl = preview _Bool <$> extractValue j xs
bothSidesEqual j Falseth (Variable xs)             = (==) (pure False) <$> vl
  where vl = preview _Bool <$> extractValue j xs
bothSidesEqual j (Variable xs) Null                = (==) (pure ()) <$> vl
  where vl = preview AL._Null <$> extractValue j xs
bothSidesEqual j Null (Variable xs)                = (==) (pure ()) <$> vl
  where vl = preview AL._Null <$> extractValue j xs
bothSidesEqual j (Variable xs) Nil                 = (==) (pure ()) <$> vl
  where vl = preview AL._Null <$> extractValue j xs
bothSidesEqual j Nil (Variable xs)                 = (==) (pure ()) <$> vl
  where vl = preview AL._Null <$> extractValue j xs
bothSidesEqual _ l r                               =
  _Failure # [ ImpossibleComparison (renderExpr l) (renderExpr r) ]

-- | Fold over multiple layers of variable syntax, and deal with future event nesting
extractValue
  :: Value
  -> JsonVarPath
  -> Rendering Value
extractValue j xz@(ObjectIndex "user" :| _) =
  case j ^? buildLens xz of
    Just v  -> _Success # v
    Nothing -> _Failure # [ JsonValueNotFound xz ]
extractValue j xz@(ObjectIndex "event" :| _) =
  case (j ^? buildLens xz) of
    Just v  -> _Success # v
    Nothing -> _Failure # [ JsonValueNotFound xz ]
extractValue j xz = -- If template doesn't have context yet - add it after first attempting raw key
  case (j ^? buildLens xz) <|>
       (j ^? buildLens (ObjectIndex "event" <| xz)) of
    Just v  -> _Success # v
    Nothing -> _Failure # [ JsonValueNotFound xz ]

-- | Apply a filter to the input
applyFilter
  :: Value
  -> Expr
  -> Rendering Text
applyFilter _ (Filter (QuoteString q) [])      = pure q
applyFilter _ (Filter (QuoteString q) (c:fcs)) =
  case applyFilterM q c >>= \i -> foldM' applyFilterM i fcs of
    Just t -> AccSuccess t
    _      -> _Failure # [ RenderingFailure "Filtration fn failure" ]
applyFilter j (Filter (Variable vs) fcs)   =
    case res of
      (AccSuccess (Just t)) -> AccSuccess t
      failure -> _Failure # [ RenderingFailure "Variable filtration fn failure" ] <*> failure
  where res = (\v -> applyCellsM v fcs) <$> extractValue j vs
applyFilter _ _                            =
  _Failure # [ LiquidError "Filter Bug!" ]

-- | Apply a chain of functions from l -> r
applyCellsM
  :: Value
  -> [Expr]
  -> Maybe Text
applyCellsM v []      = v ^? _String
applyCellsM v (c:fcs) = arrayFilterM v c >>= \i -> foldM' applyFilterM i fcs

-- | Apply a filter
applyFilterM
  :: Text -- ^ LHS
  -> Expr -- ^ FilterCell
  -> Maybe Text
applyFilterM i (FilterCell "toUpper" []) = pure $ T.toUpper i
applyFilterM i (FilterCell "toLower" []) = pure $ T.toLower i
applyFilterM i (FilterCell "toTitle" []) = pure $ T.toTitle i
applyFilterM i (FilterCell "replace" [QuoteString find,
                                      QuoteString rep ]) =
  pure $ T.replace find rep i
applyFilterM _ _                         = Nothing

-- | Apply the array filter if the targeted value is an array, otherwise the reg filter
arrayFilterM
  :: Value
  -> Expr
  -> Maybe Text
arrayFilterM v fc | isn't _Nothing $ st = st >>= (flip applyFilterM) fc
                  | otherwise           = applyArrayFilterM arr fc
  where st  = v ^? _String
        arr = v ^.. values

-- | Apply an array filter to an array
applyArrayFilterM
  :: [Value]
  -> Expr
  -> Maybe Text
applyArrayFilterM [] (FilterCell "first" []) = pure ""
applyArrayFilterM vs (FilterCell "first" []) = (vs ^? ix 0 . _String)               <|>
                                               (formatNum <$> vs ^? ix 0 . _Number) <|>
                                               (pure "")
applyArrayFilterM [] (FilterCell "last" []) = pure ""
applyArrayFilterM vs (FilterCell "last" []) = (vs ^? _last . _String)               <|>
                                              (formatNum <$> vs ^? _last . _Number) <|>
                                              (pure "")
applyArrayFilterM [] (FilterCell "firstOrDefault" [QuoteString d]) = pure d
applyArrayFilterM [] (FilterCell "firstOrDefault" [Num d])         = pure $ formatNum d
applyArrayFilterM vs (FilterCell "firstOrDefault" _)               = (vs ^? ix 0 . _String) <|>
                                                                     (formatNum <$> vs ^? ix 0 . _Number)
applyArrayFilterM [] (FilterCell "lastOrDefault" [QuoteString d]) = pure d
applyArrayFilterM [] (FilterCell "lastOrDefault" [Num d])         = pure $ formatNum d
applyArrayFilterM vs (FilterCell "lastOrDefault" _)               = (vs ^? _last . _String) <|>
                                                                    (formatNum <$> vs ^? _last . _Number)
applyArrayFilterM [] (FilterCell "toSentenceWithSeparator" _)                 = pure ""
applyArrayFilterM vs (FilterCell "toSentenceWithSeparator" [QuoteString sep, QuoteString fin]) = do
  (upToLast, lastElem) <- vs^?_Snoc
  case null upToLast of
    True -> renderv lastElem
    False -> do
      text <- mconcat . intersperse sep <$> renderEachArrayElem upToLast
      fmap (mappend $ text <> fin) $ renderv lastElem
applyArrayFilterM [] (FilterCell "renderWithSeparator" _)                 = pure ""
applyArrayFilterM vs (FilterCell "renderWithSeparator" [QuoteString sep]) =
  mconcat . intersperse sep <$> renderEachArrayElem vs
applyArrayFilterM [] (FilterCell "countElements" _) = pure "0"
applyArrayFilterM vs (FilterCell "countElements" _) = pure . T.pack . show $ length vs
applyArrayFilterM _  _ = Nothing

renderv
  :: Value
  -> Maybe Text
renderv v =
  v^?_String <|> (formatNum <$> v^?_Number)

-- | Render each array element (can only contain strings or numbers!)
renderEachArrayElem
  :: [Value]
  -> Maybe [Text]
renderEachArrayElem = traverse renderv

attachContext :: JsonVarPath -> Value -> Value -> Either Text Value
attachContext (ObjectIndex k :| []) v (Object o) = pure $ Object $ o & at k ?~ v
attachContext _ _ _ = Left "New variable name must be a text."

idContext :: Value -> (Value -> Rendering a) -> Rendering (a, Value)
idContext j f = (\a -> (a, j)) <$> f j

evalValue :: Value -> Expr -> Rendering Value
evalValue j t =
  case evalTruthiness j t of
    AccSuccess b -> _Success # Bool b
    _ ->
      case t of
        Variable v -> extractValue j v
        Num n -> _Success # Number n
        QuoteString x -> _Success # String x
        Trueth -> _Success # Bool True
        Falseth -> _Success # Bool False
        _ -> _Failure # [ RenderingFailure "expects value normal form" ]
