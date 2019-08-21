{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass, DeriveFunctor, TypeFamilies #-}
{-# LANGUAGE DeriveFoldable, DeriveTraversable, PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell, StandaloneDeriving, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Grin.Value where

import Data.Binary
import Data.Data
import Data.Int
import Data.String
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Text.Printf
import Grin.Pretty hiding (SChar)


data Name = NM { getName :: !Text }
  deriving (Generic, Data, Eq, Ord)

mkName :: String -> Name
mkName = fromString

instance Show Name where
  show (NM nm) = show nm

nMap :: (Text -> Text) -> Name -> Name
nMap f (NM n) = NM (f n)

instance Semigroup  Name where (NM n1) <> (NM n2) = NM (n1 <> n2)
instance Monoid     Name where mempty = NM mempty
instance IsString   Name where fromString = NM . fromString
instance PrintfArg  Name where formatArg = formatString . Data.Text.unpack . getName

nameString :: Name -> String
nameString (NM n) = Data.Text.unpack n

-- * GRIN Tag

data TagType = C | F | P Int {-missing parameter count-}
  deriving (Generic, Data, Eq, Ord, Show)

data Tag = Tag { tagType :: TagType, tagName :: Name }
  deriving (Generic, Data, Eq, Ord, Show)

-- * GRIN Value

data SimpleValue
  = SInt64  Int64
  | SWord64 Word64
  | SFloat  Float
  | SBool   Bool
  | SChar   Char
  deriving (Generic, Data, Eq, Ord, Show)

-- | Complete node
data Node = Node Tag [Name]
  deriving (Generic, Data, Eq, Ord, Show)

data Value
  = VNode Node
  | VPrim SimpleValue
  deriving (Generic, Data, Eq, Ord, Show)

data VarOrValue
  = Var Name
  | Val Value
  deriving (Generic, Data, Eq, Ord, Show)

instance Pretty Node where
  pretty (Node tag args) = parens $ hsep (pretty tag : fmap pretty args)

instance Pretty Name where
  pretty = text . nameString

instance Pretty Value where
  pretty = \case
    VNode node   -> pretty node
    VPrim sval   -> pretty sval

instance Pretty VarOrValue where
  pretty = \case
    Var n -> pretty n
    Val v -> pretty v

instance Pretty SimpleValue where
  pretty = \case
    SInt64 a   -> integer $ fromIntegral a
    SWord64 a  -> integer (fromIntegral a) <> text "u"
    SFloat a   -> float a
    SBool a    -> text "#" <> text (show a)
    SChar a    -> text "#" <> text (show a)

instance Pretty TagType where
  pretty = green . \case
    C   -> text "C"
    F   -> text "F"
    P i -> text "P" <> int i

instance Pretty Tag where
  pretty (Tag tagtype name) = pretty tagtype <> pretty name

