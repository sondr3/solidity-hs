module Identifiers
  ( Identifier (..),
    IdentifierPath (..),
    showIdentifier,
    showIdentifierPath,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

newtype Identifier = Identifier Text deriving stock (Show, Read, Eq, Ord, Generic)

showIdentifier :: Identifier -> Text
showIdentifier (Identifier ident) = ident

newtype IdentifierPath = IdentifierPath [Identifier] deriving stock (Show, Read, Eq, Ord, Generic)

showIdentifierPath :: IdentifierPath -> Text
showIdentifierPath (IdentifierPath path) = T.intercalate "." (map showIdentifier path)
