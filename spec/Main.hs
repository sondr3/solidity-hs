module Main
  ( main,
  )
where

import Data.Text.IO qualified as TIO
import Data.Text.Lazy.IO qualified as TLIO
import Solidity (Solidity (Solidity), parseFile')
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsFile)
import Text.Pretty.Simple (pShowNoColor)

main :: IO ()
main = do
  solFiles <- findByExtension [".sol"] "spec"
  parse <- mapM mkParseTest solFiles
  let parseTest = testGroup "Parse tests" parse
  defaultMain (testGroup "Golden tests" [parseTest])

mkParseTest :: FilePath -> IO TestTree
mkParseTest path = do
  let testName = takeBaseName path
      goldenPath = replaceExtension path ".golden"
      outPath = replaceExtension path ".out"
  return (goldenVsFile testName goldenPath outPath (parse outPath))
  where
    parse :: FilePath -> IO ()
    parse outPath = do
      file <- TIO.readFile path
      case parseFile' path file of
        Right (Solidity code) -> TLIO.writeFile outPath (pShowNoColor code)
        Left err -> TIO.writeFile outPath err
      pure ()
