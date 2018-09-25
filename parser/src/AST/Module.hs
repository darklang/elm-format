{-# LANGUAGE DeriveGeneric #-}

module AST.Module
    ( Module(..), Header(..), SourceTag(..)
    , UserImport, ImportMethod(..)
    , DetailedListing(..)
    ) where

import AST.Declaration (TopLevelStructure, Declaration)
import qualified AST.Variable as Var
import qualified Cheapskate.Types as Markdown
import Data.Map.Strict (Map)
import qualified Reporting.Annotation as A
import AST.V0_16
import GHC.Generics


-- MODULES


data Module = Module
    { initialComments :: Comments
    , header :: Header
    , docs :: A.Located (Maybe Markdown.Blocks)
    , imports :: PreCommented (Map [UppercaseIdentifier] (Comments, ImportMethod))
    , body :: [TopLevelStructure Declaration]
    }
    deriving (Eq, Show, Generic)


instance A.Strippable Module where
  stripRegion m =
    Module
    { initialComments = initialComments m
    , header =
        Header
          { srcTag = srcTag $ header m
          , name = name $ header m
          , moduleSettings = moduleSettings $ header m
          , exports = exports $ header m
          }
    , docs = A.stripRegion $ docs m
    , imports = imports m
    , body = map A.stripRegion $ body m
    }


-- HEADERS

data SourceTag
  = Normal
  | Effect Comments
  | Port Comments
  deriving (Eq, Show, Generic)


{-| Basic info needed to identify modules and determine dependencies. -}
data Header = Header
    { srcTag :: SourceTag
    , name :: Commented [UppercaseIdentifier]
    , moduleSettings :: Maybe (KeywordCommented SourceSettings)
    , exports :: KeywordCommented (Var.Listing DetailedListing)
    }
    deriving (Eq, Show, Generic)


data DetailedListing = DetailedListing
    { values :: Var.CommentedMap LowercaseIdentifier ()
    , operators :: Var.CommentedMap SymbolIdentifier ()
    , types :: Var.CommentedMap UppercaseIdentifier (Comments, Var.Listing (Var.CommentedMap UppercaseIdentifier ()))
    }
    deriving (Eq, Show, Generic)


type SourceSettings =
  [(Commented LowercaseIdentifier, Commented UppercaseIdentifier)]

-- IMPORTs

type UserImport
    = (PreCommented [UppercaseIdentifier], ImportMethod)


data ImportMethod = ImportMethod
    { alias :: Maybe (Comments, PreCommented UppercaseIdentifier)
    , exposedVars :: (Comments, PreCommented (Var.Listing DetailedListing))
    }
    deriving (Eq, Show, Generic)
