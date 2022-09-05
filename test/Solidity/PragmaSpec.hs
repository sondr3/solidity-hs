module Solidity.PragmaSpec (spec) where

import Data.Text (Text)
import Solidity
import Test.Hspec
import TestUtils (testParseMany)

pragmas :: [(Text, PragmaDefinition)]
pragmas =
  [ ("pragma solidity >=0.4.0 <0.6.0;", PragmaDefinition {pragma = "solidity >=0.4.0 <0.6.0"}),
    ("pragma solidity ^0.4.0;", PragmaDefinition {pragma = "solidity ^0.4.0"}),
    ("pragma experimental SMTChecker;", PragmaDefinition {pragma = "experimental SMTChecker"})
  ]

spec :: Spec
spec = parallel $ do
  testParseMany parsePragma pragmas "Parses pragmas"
