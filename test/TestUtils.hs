module TestUtils (testParse, parseFile, testParseMany) where

import Control.Monad (forM_)
import Data.Text (Text)
import Solidity (Parser, Solidity, parseSolidity)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse, shouldSucceedOn)
import Text.Megaparsec (ParseErrorBundle, Parsec, errorBundlePretty, parse)
import Text.Printf (printf)

-- | Test utility to run a parser on some input
testParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
testParse p = parse p ""

parseFile :: (Applicative f) => Text -> f Solidity
parseFile t = case parse parseSolidity "" t of
  Right out -> pure out
  Left err -> error $ errorBundlePretty err

testParseMany :: (Show a, Eq a) => Parser a -> [(Text, a)] -> String -> Spec
testParseMany parser input title = describe title $ do
  forM_ input $ \int -> do
    let (str, typ) = int
    it (printf "parses %s" str) $ do
      testParse parser `shouldSucceedOn` str
      testParse parser str `shouldParse` typ
