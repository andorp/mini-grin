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
import Grin.Pretty


data Name = NM { unNM :: !Text }
  deriving (Generic, Data, Eq, Ord)

instance Show Name where
  show (NM nm) = show nm

nMap :: (Text -> Text) -> Name -> Name
nMap f (NM n) = NM (f n)

instance Semigroup  Name where (NM n1) <> (NM n2) = NM (n1 <> n2)
instance Monoid     Name where mempty = NM mempty
instance IsString   Name where fromString = NM . fromString
instance PrintfArg  Name where formatArg = formatString . Data.Text.unpack . unNM

nameString :: Name -> String
nameString (NM n) = Data.Text.unpack n

-- * GRIN Tag

data TagType = C | F | P Int {-missing parameter count-}
  deriving (Generic, Data, Eq, Ord, Show)

data Tag = Tag { tagType :: TagType, tagName :: Name }
  deriving (Generic, Data, Eq, Ord, Show)

-- * GRIN Literal

data Lit
  = LInt64  Int64
  | LWord64 Word64
  | LFloat  Float
  | LBool   Bool
  | LString Text
  | LChar   Char
  deriving (Generic, Data, Eq, Ord, Show)

-- * GRIN Value

type LPat = Val -- ConstTagNode, VarTagNode, ValTag, Unit, Lit, Var
type SimpleVal = Val

data Val
  = ConstTagNode  Tag  [Name]       -- complete node (constant tag) ; HIGH level GRIN
  | Unit                            -- HIGH level GRIN
  -- simple val
  | Lit Lit                         -- HIGH level GRIN
  | Var Name                        -- HIGH level GRIN
  deriving (Generic, Data, Eq, Ord, Show)

instance Pretty Name where
  pretty = text . nameString

instance Pretty Val where
  pretty = \case
    ConstTagNode tag args -> parens $ hsep (pretty tag : fmap pretty args)
    Unit         -> parens Grin.Pretty.empty
    -- simple val
    Lit lit      -> pretty lit
    Var name     -> pretty name

instance Pretty Lit where
  pretty = \case
    LInt64 a   -> integer $ fromIntegral a
    LWord64 a  -> integer (fromIntegral a) <> text "u"
    LFloat a   -> float a
    LBool a    -> text "#" <> text (show a)
    LString a  -> text "#" <> text (show a)
    LChar a    -> text "#" <> text (show a)

instance Pretty TagType where
  pretty = green . \case
    C   -> text "C"
    F   -> text "F"
    P i -> text "P" <> int i

instance Pretty Tag where
  pretty (Tag tagtype name) = pretty tagtype <> pretty name

