module Solidity.ContractSpec (spec) where

import Solidity (ContractDefinition (..), Identifier (Identifier), IdentifierPath (IdentifierPath), InheritanceSpecifier (InheritanceSpecifier), parseContract)
import Test.Hspec
import Test.Hspec.Megaparsec
import TestUtils (testParse)

spec :: Spec
spec = parallel $ do
  describe "Parses simple contracts" $ do
    it "parses empty contract" $ do
      testParse parseContract "contract TestName {}"
        `shouldParse` ContractDefinition
          { abstract = False,
            name = Identifier "TestName",
            inheritance = Nothing,
            body = []
          }
    it "parses empty abstract contract" $ do
      testParse parseContract "abstract contract _Abstract123 is Parent {}"
        `shouldParse` ContractDefinition
          { abstract = True,
            name = Identifier "_Abstract123",
            inheritance = Just [InheritanceSpecifier (IdentifierPath [Identifier "Parent"]) Nothing],
            body = []
          }
