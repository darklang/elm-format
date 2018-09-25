{-# LANGUAGE DeriveGeneric #-}

module AST.Variable where

import AST.V0_16
import Data.Map.Strict
import GHC.Generics


data Ref
    = VarRef [UppercaseIdentifier] LowercaseIdentifier
    | TagRef [UppercaseIdentifier] UppercaseIdentifier
    | OpRef SymbolIdentifier
    deriving (Eq, Ord, Show, Generic)


-- LISTINGS

-- | A listing of values. Something like (a,b,c) or (..) or (a,b,..)
data Listing a
    = ExplicitListing a Bool
    | OpenListing (Commented ())
    | ClosedListing
    deriving (Eq, Ord, Show, Generic)


type CommentedMap k v =
    Map k (Commented v)


-- | A value that can be imported or exported
data Value
    = Value !LowercaseIdentifier
    | OpValue SymbolIdentifier
    | Union (PostCommented UppercaseIdentifier) (Listing (CommentedMap UppercaseIdentifier ()))
    deriving (Eq, Ord, Show, Generic)
