module Solidity.FunctionSpec (spec) where

import Solidity
import Test.Hspec
import Test.Hspec.Megaparsec
import TestUtils (testParse)

spec :: Spec
spec = parallel $ do
  describe "Parses simple functions" $ do
    it "parses empty function" $ do
      testParse parseFunction "function test() returns (uint){}"
        `shouldParse` FunctionDefinition
          { name = Identifier "test",
            restrictions = [],
            params = [],
            returns = Just [Parameter {kind = ElementaryType (UnsignedInteger Nothing), location = Nothing, ident = Nothing}],
            body = BlockStatement []
          }
    it "parses visibility/mutability" $ do
      testParse parseFunction "function other() public pure {    }"
        `shouldParse` FunctionDefinition
          { name = Identifier "other",
            restrictions = [FuncVisibility FuncPublic, FuncMutability Pure],
            params = [],
            returns = Nothing,
            body = BlockStatement []
          }
