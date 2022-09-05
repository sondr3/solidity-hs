module Solidity.ImportSpec (spec) where

import Data.Text (Text)
import Solidity
import Test.Hspec
import TestUtils (testParseMany)

pragmas :: [(Text, ImportDefinition)]
pragmas =
  [ ("import \"filename\";", ImportDefinition (ImportPath "filename" Nothing)),
    ("import * as symbolName from \"filename\";", ImportDefinition (ImportWildcard (Identifier "symbolName") "filename")),
    ("import \"filename\" as symbolName;", ImportDefinition (ImportPath "filename" (Just $ Identifier "symbolName"))),
    ("import {symbol1 as aliasName, symbol2} from \"filename\";", ImportDefinition (ImportAliases [(Identifier "symbol1", Just $ Identifier "aliasName"), (Identifier "symbol2", Nothing)] "filename"))
  ]

spec :: Spec
spec = parallel $ do
  testParseMany parseImport pragmas "Parses imports"
