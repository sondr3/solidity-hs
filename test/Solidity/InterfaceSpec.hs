module Solidity.InterfaceSpec (spec) where

import Solidity (Identifier (Identifier), InterfaceDefinition (..), parseInterface)
import Test.Hspec
import Test.Hspec.Megaparsec
import TestUtils (testParse)

spec :: Spec
spec = parallel $ do
  describe "Parses simple interface" $ do
    it "parses empty interface" $ do
      let res = testParse parseInterface "interface TestName {}"
      res `parseSatisfies` ((== Identifier "TestName") . name)
