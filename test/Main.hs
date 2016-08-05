import           Test.Tasty (defaultMain, TestTree, testGroup)
import           Text.Liquid.HelperTests
import           Text.Liquid.ParserTests
import           Text.Liquid.RendererTests
import           Text.Liquid.VariableFinderTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Root"
                  [ helperTests
                  , parserTests
                  , rendererTests
                  , variableFinderTests
                  ]
