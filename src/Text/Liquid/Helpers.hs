{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Text.Liquid.Helpers (
    foldM'
  , formatNum
  , buildLens
  , renderPath
  , renderExpr
  )where

import           Data.Aeson.Lens   (key, nth)
import           Data.Aeson.Types  (Value)
import           Data.List         (intersperse)
import           Data.Monoid
import           Data.Scientific
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Text.Liquid.Types


-- | Strict monadic foldl
foldM'
  :: Monad m
  => (a -> b -> m a)
  -> a
  -> [b]
  -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs

-- | Format a number
formatNum
  :: Scientific
  -> Text
formatNum s | isInteger s =
  maybe T.empty (T.pack . show) (toBoundedInteger s :: Maybe Int)
            | otherwise   =
  T.pack $ formatScientific Fixed Nothing s

-- | Compose a traversal into a JSON Value
buildLens
  :: forall (f :: * -> *) . Applicative f
  => JsonVarPath
  -> ((Value -> f Value) -> Value -> f Value)
buildLens xs = foldl1 (.) (matchKey <$> xs)
  where matchKey (ObjectIndex i) = key i
        matchKey (ArrayIndex i)  = nth i

renderPath
  :: JsonVarPath
  -> Text
renderPath = foldl conc T.empty
  where conc :: Text -> VarIndex -> Text
        conc l (ObjectIndex r) | T.null l  = r
                               | otherwise = l <> "." <> r
        conc l (ArrayIndex r) | T.null l   = "[" <> T.pack (show r) <> "]"
                              | otherwise  = l <> "[" <> T.pack (show r) <> "]"

renderExpr
  :: Expr
  -> Text
renderExpr e = case e of
  Noop                -> mempty
  (RawText t)         -> t
  (Num s)             -> formatNum s
  (Variable jp)       -> renderPath jp
  (QuoteString t)     -> "\'" <> t <> "\'"
  (Equal l r)         -> renderExpr l <> " == " <> renderExpr r
  (NotEqual l r)      -> renderExpr l <> " != " <> renderExpr r
  (GtEqual l r)       -> renderExpr l <> " >= " <> renderExpr r
  (LtEqual l r)       -> renderExpr l <> " <= " <> renderExpr r
  (Gt l r)            -> renderExpr l <> " > " <> renderExpr r
  (Lt l r)            -> renderExpr l <> " < " <> renderExpr r
  (Or l r)            -> renderExpr l <> " or " <> renderExpr r
  (And l r)           -> renderExpr l <> " and " <> renderExpr r
  (Contains l r)      -> renderExpr l <> " contains " <> renderExpr r
  Trueth              -> "true"
  Falseth             -> "false"
  Nil                 -> "nil"
  Null                -> "null"
  (Truthy x)          -> renderExpr x
  (IfClause x)        -> "{% if " <> renderExpr x <> " %}"
  (IfKeyClause x)     -> "{% ifkey " <> renderExpr x <> " %}"
  (ElsIfClause x)     -> "{% elsif " <> renderExpr x <> " %}"
  Else                -> "{% else %}"
  (FilterCell n [])   -> n
  (FilterCell n opts) -> n <> ": " <> mconcat (intersperse ", " $ renderExpr <$> opts)
  (Filter t fs)       -> renderExpr t <> " | " <> mconcat (intersperse " | " $ renderExpr <$> fs)
  (Output x)          -> "{{ " <> renderExpr x <>  " }}"
  (TrueStatements xs) -> mconcat $ renderExpr <$> xs
  (IfLogic i
           ts@(TrueStatements _)) ->
    renderExpr i  <>
    renderExpr ts <>
    "{% endif %}"
  (IfLogic (IfLogic i ts1@(TrueStatements _))
                    (IfLogic Else ts2@(TrueStatements _))) ->
    renderExpr i    <>
    renderExpr ts1  <>
    renderExpr Else <>
    renderExpr ts2  <>
    "{% endif %}"
  (IfLogic (IfLogic i ts1@(TrueStatements _))
                    (IfLogic ei@(ElsIfClause _) ts2@(TrueStatements _))) ->
    renderExpr i   <>
    renderExpr ts1 <>
    renderExpr ei  <>
    renderExpr ts2 <>
    "{% endif %}"
  (IfLogic (IfLogic i ts1@(TrueStatements _))
                    (IfLogic (IfLogic ei@(ElsIfClause _) ts2@(TrueStatements _))
                             (IfLogic Else ts3@(TrueStatements _)))) ->
    renderExpr i    <>
    renderExpr ts1  <>
    renderExpr ei   <>
    renderExpr ts2  <>
    renderExpr Else <>
    renderExpr ts3  <>
    "{% endif %}"
  (CaseLogic s ts) ->
    "{% case " <> renderExpr s <> " %}" <>
    mconcat (renderCaseLogic <$> ts)    <>
    "{% endcase %}"
  _ -> mempty

renderCaseLogic
  :: (Expr, Expr)
  -> Text
renderCaseLogic (Else, y) =
  "{% else %}" <> renderExpr y
renderCaseLogic (x, y)    =
  "{% when " <> renderExpr x <> " %}" <> renderExpr y

