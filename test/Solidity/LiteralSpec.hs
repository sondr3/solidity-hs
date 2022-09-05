module Solidity.LiteralSpec (spec) where

import Solidity
import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)
import TestUtils (testParse)

spec :: Spec
spec = parallel $ do
  it "parses boolean literals" $ do
    testParse parseLiteral "true" `shouldParse` BooleanLiteral True
    testParse parseLiteral "false" `shouldParse` BooleanLiteral False
  it "parses string literals" $ do
    testParse parseLiteral "\"hello world!\"" `shouldParse` StringLiteral "hello world!"
    testParse parseLiteral "'this is another string'" `shouldParse` StringLiteral "this is another string"
  it "parses number literals" $ do
    testParse parseLiteral "0xDEADBEEF" `shouldParse` NumberLiteral 3735928559 Nothing
    testParse parseLiteral "17461" `shouldParse` NumberLiteral 17461 Nothing
    testParse parseLiteral "2e10" `shouldParse` NumberLiteral 2e10 Nothing
    testParse parseLiteral "2.5e1" `shouldParse` NumberLiteral 2.5e1 Nothing
    testParse parseLiteral "2e-10" `shouldParse` NumberLiteral 2e-10 Nothing
  it "parses number literals with units" $ do
    testParse parseLiteral "0xDEADBEEF wei" `shouldParse` NumberLiteral 3735928559 (Just Wei)
    testParse parseLiteral "17461 seconds" `shouldParse` NumberLiteral 17461 (Just Seconds)
    testParse parseLiteral "2e10 ether" `shouldParse` NumberLiteral 2e10 (Just Ether)
    testParse parseLiteral "2e10 minutes" `shouldParse` NumberLiteral 2e10 (Just Minutes)
    testParse parseLiteral "2.5e1 gwei" `shouldParse` NumberLiteral 2.5e1 (Just Gwei)
    testParse parseLiteral "2e-10 years" `shouldParse` NumberLiteral 2e-10 (Just Years)
  it "parses hex string literals" $ do
    testParse parseLiteral "hex\"001122FF\"" `shouldParse` HexStringLiteral "001122FF"
    testParse parseLiteral "hex\"0011_22_FF\"" `shouldParse` HexStringLiteral "0011_22_FF"
    testParse parseLiteral "hex'001122FF'" `shouldParse` HexStringLiteral "001122FF"
    testParse parseLiteral "hex'0011_22_FF'" `shouldParse` HexStringLiteral "0011_22_FF"
  it "parses unicode string literals" $ do
    testParse parseLiteral "unicode\"Hello 😃\"" `shouldParse` UnicodeStringLiteral "Hello 😃"
    testParse parseLiteral "unicode'Hello 😃'" `shouldParse` UnicodeStringLiteral "Hello 😃"
