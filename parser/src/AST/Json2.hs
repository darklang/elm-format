{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module AST.Json2 where

import AST.Declaration
import AST.Expression
import AST.Module
import AST.Pattern
import AST.Variable
import AST.V0_16
import Reporting.Annotation hiding (map)
import Data.Sequence
import Data.Foldable
import Data.Aeson
import GHC.Generics

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified ElmFormat.Version
import qualified Reporting.Region as Region
import Cheapskate.Types

pleaseReport :: String -> String -> a
pleaseReport what details =
    error ("<elm-format-" ++ ElmFormat.Version.asString ++ ": "++ what ++ ": " ++ details ++ " -- please report this at https://github.com/avh4/elm-format/issues >")

instance ToJSON (Cheapskate.Types.Block)
instance ToJSON (Cheapskate.Types.CodeAttr)
instance ToJSON (Cheapskate.Types.ListType)
instance ToJSON (Cheapskate.Types.NumWrapper)
instance ToJSON (Cheapskate.Types.HtmlTagType)
instance ToJSON (Cheapskate.Types.Inline)
instance ToJSON (Cheapskate.Types.LinkTarget)
instance ToJSON (Cheapskate.Types.Options)

instance (ToJSON a, ToJSON b) => ToJSON (Pair a b)

instance (ToJSON a) => ToJSON (Commented a)
instance (ToJSON a) => ToJSON (KeywordCommented a)
instance (ToJSON a) => ToJSON (Listing a)
instance (ToJSON a) => ToJSON (TopLevelStructure a)
instance (ToJSON a) => ToJSON (Located a)
instance (ToJSON a) => ToJSON (OpenCommentedList a)

instance ToJSON UppercaseIdentifier
instance ToJSON LowercaseIdentifier
instance ToJSON SymbolIdentifier
instance ToJSON TypeConstructor
instance ToJSON Module
instance ToJSON Literal
instance ToJSON Ref
instance ToJSON Type'
instance ToJSON Expr'
instance ToJSON Assoc
instance ToJSON SourceTag
instance ToJSON FloatRepresentation
instance ToJSON ImportMethod
instance ToJSON Region.Region
instance ToJSON Region.Position
instance ToJSON DetailedListing
instance ToJSON Header
instance ToJSON Comment
-- instance ToJSON Expr
instance ToJSON LetDeclaration
instance ToJSON Declaration
instance ToJSON IntRepresentation
instance ToJSON Pattern'
instance ToJSON UnaryOperator
instance ToJSON FunctionApplicationMultiline
instance ToJSON ForceMultiline
instance ToJSON Multiline

instance ToJSONKey UppercaseIdentifier
instance ToJSONKey SymbolIdentifier
instance ToJSONKey LowercaseIdentifier
