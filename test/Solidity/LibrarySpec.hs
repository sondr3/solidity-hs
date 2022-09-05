module Solidity.LibrarySpec (spec) where

import Solidity (Identifier (Identifier), LibraryDefinition (..), parseLibrary)
import Test.Hspec
import Test.Hspec.Megaparsec
import TestUtils (testParse)

spec :: Spec
spec = parallel $ do
  describe "Parses simple library" $ do
    it "parses empty library" $ do
      testParse parseLibrary "library TestName {}"
        `shouldParse` LibraryDefinition
          { name = Identifier "TestName",
            body = []
          }
