import           Criterion.Main
import           Data.Aeson                 hiding (Result)
import           Data.Attoparsec.Text
import           Data.Text                  (Text)
import           Text.Liquid.Parser
import           Text.Liquid.Renderer
import           Text.Liquid.Types
import           Text.Liquid.VariableFinder

template :: Text
template = "some text prior {{ a.nested[2] | replace:\'foo\',\'bar\' | toUpper }}somesquishedtext{% if user == \'barry\' %} then this text {% else %} yo yo {% endif %}the end!{% case user %}{% when \'notbarry\' %} no barrys {% when \'barry\' %} t'is barry!{% else %} any old nonsense{% endcase %}"

jsonCtx :: Value
jsonCtx = object ["a" .= nested, "user" .= ("barry" :: Text)]
  where nested = object ["nested" .= ([ "a", "b", "foobar" ] :: [Text])]

parsedTemplate :: Maybe [Expr]
parsedTemplate = maybeResult $ parseTemplate template

{-# INLINE benchParseTemplate #-}
benchParseTemplate :: Text -> Maybe [Expr]
benchParseTemplate t = maybeResult $ parseTemplate t

{-# INLINE benchRenderTemplate #-}
benchRenderTemplate :: (Value, Maybe [Expr]) -> Maybe [Rendering Text]
benchRenderTemplate (v, ts) = fmap (renderTemplate v) <$> ts

{-#INLINE benchInterpret #-}
benchInterpret :: (Value, Text) -> Rendering Text
benchInterpret (j, t) = interpretWithJson j t

{-#INLINE benchFindVariables #-}
benchFindVariables :: Maybe [Expr] -> Maybe [(JsonVarPath, VType)]
benchFindVariables xs = findAllVariables <$> xs

main :: IO ()
main = defaultMain [
         bgroup "Text.Liquid" [ bench "parse-whnf" $ whnf benchParseTemplate template
                              , bench "renderTemplate"  $ whnf benchRenderTemplate (jsonCtx, parsedTemplate)
                              , bench "interpret" $ whnf benchInterpret (jsonCtx, template)
                              , bench "findAllVariables" $ whnf benchFindVariables parsedTemplate
                              ]]

