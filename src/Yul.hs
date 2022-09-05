{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Yul where

import Data.Text (Text)
import GHC.Generics (Generic)
import Identifiers

data YulStatement
  = YulBlock [YulStatement]
  | YulVariableStatement YulVariableDeclaration
  | YulAssignment YulAssignmentDeclaration
  | YulFunctionCall YulFunctionCallDeclaration
  | YulIfStatement YulIfStatementDeclaration
  | YulForStatement YulForStatementDeclaration
  | YulSwitchStatement YulSwitchStatementDeclaration
  | YulLeave
  | YulBreak
  | YulContinue
  | YulFunctionDefinition YulFunctionDefinitionDeclaration
  deriving stock (Show, Read, Eq, Ord, Generic)

data YulIdentifier = YulIdentifier Identifier | YulEvmBuiltin Identifier
  deriving stock (Show, Read, Eq, Ord, Generic)

newtype YulIdentifierPath = YulIdentifierPath [YulIdentifier] deriving stock (Show, Read, Eq, Ord, Generic)

data YulVariableDeclaration
  = YulVariableDeclaration YulIdentifier YulExpression
  | YulMultipleVariableDeclaration [YulIdentifier] YulFunctionCallDeclaration
  deriving stock (Show, Read, Eq, Ord, Generic)

data YulAssignmentDeclaration
  = YulAssignmentDeclaration YulIdentifierPath YulExpression
  | YulMultipleAssignmentDeclaration [YulIdentifierPath] YulFunctionCallDeclaration
  deriving stock (Show, Read, Eq, Ord, Generic)

data YulIfStatementDeclaration = YulIfStatementDeclaration
  { expr :: YulExpression,
    body :: YulStatement
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data YulForStatementDeclaration = YulForStatementDeclaration
  { initializer :: YulStatement,
    condition :: YulExpression,
    increment :: YulStatement,
    body :: YulStatement
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data YulSwitchStatementDeclaration = YulSwitchStatementDeclaration
  { expr :: YulExpression,
    cases :: [(YulLiteral, YulStatement)],
    defaultBlock :: Maybe YulStatement
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data YulFunctionDefinitionDeclaration = YulFunctionDefinitionDeclaration
  { ident :: YulIdentifier,
    params :: [YulIdentifier],
    returns :: Maybe [YulIdentifier],
    body :: YulStatement
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data YulFunctionCallDeclaration = YulFunctionCallDeclaration
  { ident :: YulIdentifier,
    body :: [YulExpression]
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data YulExpression
  = YulExpressionPath YulIdentifierPath
  | YulExpressionFunctionCall YulFunctionCallDeclaration
  | YulExpressionLiteral YulLiteral
  deriving stock (Show, Read, Eq, Ord, Generic)

data YulLiteral
  = YulDecimalNumber Integer
  | YulStringLiteral Text
  | YulHexNumber Integer
  | YulBoolean Bool
  | YulHexString Text
  deriving stock (Show, Read, Eq, Ord, Generic)
