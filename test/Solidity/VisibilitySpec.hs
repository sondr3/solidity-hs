module Solidity.VisibilitySpec (spec) where

import Data.Text (Text)
import Solidity (FunctionVisibility (..), parseVisibility)
import Test.Hspec
import TestUtils (testParseMany)

visibility :: [(Text, FunctionVisibility)]
visibility =
  [ ("private", FuncPrivate),
    ("public", FuncPublic),
    ("internal", FuncInternal),
    ("external", FuncExternal)
  ]

spec :: Spec
spec = parallel $ do
  testParseMany parseVisibility visibility "Parses visibility"
