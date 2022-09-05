{-# LANGUAGE OverloadedLabels #-}

module PrettyPrint
  ( prettyPrintLn,
    prettyPrint,
  )
where

import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Identifiers
import Optics (coerced, view, (^.))
import Types

class PrettyPrint a where
  pretty :: a -> Int -> Text

indent :: Int -> Text -> Text
indent i it = T.replicate (i * 2) " " <> it <> "\n"

instance PrettyPrint [(TypeName, Identifier)] where
  pretty xs ind = indent ind ("[" <> T.intercalate "," (map (\t -> "(" <> pretty t 0 <> ")") xs))

instance PrettyPrint (TypeName, Identifier) where
  pretty (t, i) _ = showTypeName t <> ", " <> showIdentifier i

instance PrettyPrint (Identifier, Maybe Identifier) where
  pretty (ident, Just odent) _ = showIdentifier ident <> " " <> showIdentifier odent
  pretty (ident, Nothing) _ = showIdentifier ident

instance PrettyPrint ImportType where
  pretty (ImportPath path ident) ind = indent ind $ path <> maybe "" (\x -> " as " <> showIdentifier x) ident
  pretty (ImportAliases xs path) ind = indent ind $ T.intercalate " " (map (`pretty` ind) xs) <> " from " <> path
  pretty (ImportWildcard ident path) ind = indent ind $ "* as " <> showIdentifier ident <> " from " <> path

instance PrettyPrint ConstructorDefinition where
  pretty con ind = do
    let f = indent ind "Constructor []"
    f <> pretty (con ^. #body) (ind + 1)

instance PrettyPrint FunctionDefinition where
  pretty fun ind = do
    let f = indent ind $ "Function [" <> view coerced (fun ^. #name) <> "]"
    f <> pretty (fun ^. #body) (ind + 1)

instance PrettyPrint StructDefinition where
  pretty struct ind = do
    let f = indent ind $ "Struct [" <> view coerced (struct ^. #ident) <> "]"
    f <> pretty (struct ^. #members) (ind + 1)

instance PrettyPrint EnumDefinition where
  pretty enum ind = do
    let f = indent ind $ "Enum [" <> view coerced (enum ^. #ident) <> "]"
        body = foldl' (\acc s -> acc <> showIdentifier s) "" (enum ^. #members)
    f <> body

instance PrettyPrint CatchClause where
  pretty c ind = do
    let f = indent ind "Catch"
    f <> pretty (c ^. #body) (ind + 1)

instance PrettyPrint TryStatement where
  pretty try ind = do
    let f = indent ind "Try"
        body = pretty (try ^. #body) (ind + 1)
        catches = foldl' (\acc s -> acc <> pretty s (ind + 1)) "" (try ^. #catch)
    f <> body <> catches

instance PrettyPrint UserDefinedValueDefinition where
  pretty val ind = indent ind $ showIdentifier (val ^. #ident) <> " is " <> showElementayType (val ^. #kind)

instance PrettyPrint ModifierDefinition where
  pretty m ind = indent ind $ "Modifier [" <> view coerced (m ^. #ident) <> "]"

instance PrettyPrint EventDefinition where
  pretty event ind = indent ind $ "Event [" <> view coerced (event ^. #ident) <> "]"

instance PrettyPrint ErrorDefinition where
  pretty err ind = indent ind $ "Error [" <> view coerced (err ^. #ident) <> "]"

instance PrettyPrint ConstantVarDeclaration where
  pretty con ind = indent ind $ "Error [" <> view coerced (con ^. #ident) <> "]"

instance PrettyPrint SourceUnit where
  pretty (Pragma p) ind = indent ind $ "Pragma [" <> p ^. #pragma <> "]"
  pretty (Import (ImportDefinition imp)) ind = indent ind $ "Import [" <> pretty imp ind <> "]"
  pretty (Contract contract) ind = do
    let c = indent ind $ "Contract [" <> view coerced (contract ^. #name) <> "]"
        body = foldl' (\acc s -> acc <> pretty s (ind + 1)) "" (contract ^. #body)
    c <> body
  pretty (Interface interface) ind = do
    let i = indent ind $ "Interface [" <> view coerced (interface ^. #name) <> "]"
        body = foldl' (\acc s -> acc <> pretty s (ind + 1)) "" (interface ^. #body)
    i <> body
  pretty (Library lib) ind = do
    let i = indent ind $ "Library [" <> view coerced (lib ^. #name) <> "]"
        body = foldl' (\acc s -> acc <> pretty s (ind + 1)) "" (lib ^. #body)
    i <> body
  pretty (Function fun) ind = pretty fun ind
  pretty (ConstVar var) ind = pretty var ind
  pretty (Struct struct) ind = pretty struct ind
  pretty (Enum enum) ind = pretty enum ind
  pretty (UserDefinedType utype) ind = indent ind (T.pack $ show utype)
  pretty (Error err) ind = pretty err ind

instance PrettyPrint ContractBodyElement where
  pretty (Constructor con) ind = pretty con ind
  pretty (CFunction fun) ind = pretty fun ind
  pretty (CModifier m) ind = pretty m ind
  pretty (CFallbackFunction _) ind = indent ind "FallbackFunc"
  pretty (CReceiveFunction _) ind = indent ind "ReceiveFunc"
  pretty (CStruct struct) ind = pretty struct ind
  pretty (CEnum enum) ind = pretty enum ind
  pretty (CUserDefinedType kind) ind = pretty kind ind
  pretty (CStateVariableDec var) ind = indent ind (T.intercalate " " [showTypeName (var ^. #kind), showIdentifier (var ^. #ident), "=", maybe "" showExpression (var ^. #expr)])
  pretty (CEvent event) ind = pretty event ind
  pretty (CError err) ind = pretty err ind
  pretty (CUsing _) ind = indent ind "using"

instance PrettyPrint Statement where
  pretty (BlockStatement stmt) ind = foldl' (\acc s -> acc <> pretty s (ind + 1)) "" stmt
  pretty (If i) ind = do
    let ex = " (" <> showExpression (i ^. #expr) <> ") "
        trueBody = pretty (i ^. #trueStmt) (ind + 1) -- foldl' (\acc s -> acc <> pretty s (ind + 1)) "" (i ^. #trueStmt)
        elseBody = case i ^. #elseStmt of
          Just body -> indent ind "Else []" <> pretty body (ind + 1)
          Nothing -> ""
    indent ind ("If" <> ex) <> trueBody <> elseBody
  pretty (For f) ind = indent ind "For" <> pretty (f ^. #body) (ind + 1)
  pretty (While w) ind = do
    let ex = " (" <> showExpression (w ^. #expr) <> ") "
    indent ind ("While" <> ex) <> pretty (w ^. #stmt) (ind + 1)
  pretty (DoWhile w) ind = do
    let ex = " (" <> showExpression (w ^. #expr) <> ") "
    indent ind ("DoWhile" <> ex) <> pretty (w ^. #stmt) (ind + 1)
  pretty (ExpressionStatement expr) ind = pretty expr ind
  pretty Continue ind = indent ind "continue"
  pretty Break ind = indent ind "break"
  pretty (Return (Just expr)) ind = indent ind ("return " <> showExpression expr)
  pretty (Return Nothing) ind = indent ind "return"
  pretty (VariableStatement (VariableDecStatement var (Just expr))) ind = indent ind (showVariableDeclaration var <> " = " <> showExpression expr)
  pretty (VariableStatement (VariableDecStatement var Nothing)) ind = indent ind (showVariableDeclaration var)
  pretty (VariableStatement (VariableDecTupleStatement vars expr)) ind = indent ind (T.intercalate ", " (map (maybe "" showVariableDeclaration) vars) <> " = " <> showExpression expr)
  pretty (Try try) ind = pretty try ind
  pretty (Emit (EmitStatement expr)) ind = indent ind ("Emit " <> showExpression expr)
  pretty (Revert (RevertStatement expr)) ind = indent ind ("Revert " <> showExpression expr)
  pretty (Assembly _) ind = indent ind "assembly block"

instance PrettyPrint Expression where
  pretty expr ind = indent ind (showExpression expr)

prettyPrint :: PrettyPrint a => [a] -> Text
prettyPrint = foldl' (\acc s -> acc <> pretty s 0) ""

prettyPrintLn :: PrettyPrint a => [a] -> IO ()
prettyPrintLn syntax = TIO.putStrLn $ prettyPrint syntax
