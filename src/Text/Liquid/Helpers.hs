{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

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
foldM' :: Monad m
       => (a -> b -> m a)
       -> a
       -> [b]
       -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs

-- | Format a number
formatNum :: Scientific
          -> Text
formatNum s | isInteger s =
  maybe T.empty (T.pack . show) (toBoundedInteger s :: Maybe Int)
            | otherwise   =
  T.pack $ formatScientific Fixed Nothing s

-- | Compose a traversal into a JSON Value
buildLens :: forall (f :: * -> *) . Applicative f
          => JsonVarPath
          -> ((Value -> f Value) -> Value -> f Value)
buildLens xs = foldl1 (.) (matchKey <$> xs)
  where matchKey (ObjectIndex i) = key i
        matchKey (ArrayIndex i)  = nth i

renderPath :: JsonVarPath
           -> Text
renderPath path = foldl conc T.empty path
  where conc :: Text -> VarIndex -> Text
        conc l (ObjectIndex r) | T.null l  = r
                               | otherwise = l <> "." <> r
        conc l (ArrayIndex r) | T.null l   = "[" <> (T.pack $ show r) <> "]"
                              | otherwise  = l <> "[" <> (T.pack $ show r) <> "]"

renderExpr :: Expr
           -> Text
renderExpr Noop                = mempty
renderExpr (RawText t)         = t
renderExpr (Num s)             = formatNum s
renderExpr (Variable jp)       = renderPath jp
renderExpr (QuoteString t)     = "\'" <> t <> "\'"
renderExpr (Equal l r)         = (renderExpr l) <> " == " <> (renderExpr r)
renderExpr (NotEqual l r)      = (renderExpr l) <> " != " <> (renderExpr r)
renderExpr (GtEqual l r)       = (renderExpr l) <> " >= " <> (renderExpr r)
renderExpr (LtEqual l r)       = (renderExpr l) <> " <= " <> (renderExpr r)
renderExpr (Gt l r)            = (renderExpr l) <> " > " <> (renderExpr r)
renderExpr (Lt l r)            = (renderExpr l) <> " < " <> (renderExpr r)
renderExpr (Or l r)            = (renderExpr l) <> " or " <> (renderExpr r)
renderExpr (And l r)           = (renderExpr l) <> " and " <> (renderExpr r)
renderExpr (Contains l r)      = (renderExpr l) <> " contains " <> (renderExpr r)
renderExpr Trueth              = "true"
renderExpr Falseth             = "false"
renderExpr Nil                 = "nil"
renderExpr Null                = "null"
renderExpr (Truthy x)          = renderExpr x
renderExpr (IfClause x)        = "{% if " <> renderExpr x <> " %}"
renderExpr (IfKeyClause x)     = "{% ifkey " <> renderExpr x <> " %}"
renderExpr (ElsIfClause x)     = "{% elsif " <> renderExpr x <> " %}"
renderExpr Else                = "{% else %}"
renderExpr (FilterCell n [])   = n
renderExpr (FilterCell n opts) = n <> ": " <> (mconcat $ intersperse ", " $ renderExpr <$> opts)
renderExpr (Filter t fs)       = renderExpr t <> bar <> (mconcat $ intersperse bar $ renderExpr <$> fs)
  where bar = " | "
renderExpr (Output x)          = "{{ " <> renderExpr x <>  " }}"
renderExpr (TrueStatements xs) = mconcat $ renderExpr <$> xs
renderExpr (IfLogic i
                    ts@(TrueStatements _)) =
  renderExpr i  <>
  renderExpr ts <>
  "{% endif %}"
renderExpr (IfLogic (IfLogic i ts1@(TrueStatements _))
                    (IfLogic Else ts2@(TrueStatements _))) =
  renderExpr i    <>
  renderExpr ts1  <>
  renderExpr Else <>
  renderExpr ts2  <>
  "{% endif %}"
renderExpr (IfLogic (IfLogic i ts1@(TrueStatements _))
                    (IfLogic ei@(ElsIfClause _) ts2@(TrueStatements _))) =
  renderExpr i   <>
  renderExpr ts1 <>
  renderExpr ei  <>
  renderExpr ts2 <>
  "{% endif %}"
renderExpr (IfLogic (IfLogic i ts1@(TrueStatements _))
                    (IfLogic (IfLogic ei@(ElsIfClause _) ts2@(TrueStatements _))
                             (IfLogic Else ts3@(TrueStatements _)))) =
  renderExpr i    <>
  renderExpr ts1  <>
  renderExpr ei   <>
  renderExpr ts2  <>
  renderExpr Else <>
  renderExpr ts3  <>
  "{% endif %}"
renderExpr (CaseLogic s ts)    = "{% case " <> renderExpr s <> " %}" <>
                                 (mconcat $ renderCaseLogic <$> ts)  <>
                                 "{% endcase %}"
  where renderCaseLogic :: (Expr, Expr) -> Text
        renderCaseLogic (Else, y) = "{% else %}" <> renderExpr y
        renderCaseLogic (x, y)    = "{% when " <> renderExpr x <> " %}" <> renderExpr y
renderExpr _                   = mempty

