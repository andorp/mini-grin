{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass, DeriveFunctor, TypeFamilies #-}
{-# LANGUAGE DeriveFoldable, DeriveTraversable, PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell, StandaloneDeriving, OverloadedStrings #-}
{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Grin.Exp
  ( module Grin.Exp
  ) where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Grin.Pretty
import Grin.TypeEnv
import Grin.Value
import Prelude hiding (exp)
import Data.Map (Map, fromList)


-- * GRIN Expression

type SimpleExp  = Exp -- Meant to be SApp, SPure, SStore, SFetch, SUpdate constructors
type Alt        = Exp -- Meant to be the Alt constructor
type Def        = Exp -- Meant to be the Def constructor
type Program    = Exp -- Meant to be the Program constructor

data Exp
  = Program     [External] [Def]
  | Def         Name [Name] Exp
  -- Exp
  | EBind       Exp BPat Exp
  | ECase       Name [Alt]
  -- Simple Exp
  | SApp        Name [Name]
  | SPure       VarOrValue
  | SStore      Name -- Variable should hold only nodes
  | SFetch      Name -- Variable should hold only locations
  | SUpdate     Name Name -- The variables in order should hold only location and node
  -- Alt
  | Alt CPat Exp
  | Block Exp -- Block plays a role in transformations. When a transformation needs to
              -- replace a simple Expression with a complex Bind one, the Block constructor
              -- comes into the picture.
  deriving (Eq, Ord, Show)

-- * Externals

data External
  = External
  { eName       :: Name
  , eRetType    :: Ty
  , eArgsType   :: [Ty]
  , eEffectful  :: Bool
  }
  deriving (Eq, Ord, Show)

isExternalName :: [External] -> Name -> Bool
isExternalName es n = n `Prelude.elem` (eName <$> es)

-- * Case Pattern

-- | Case patterns that can be found in the Alt expressions.
data CPat
  = NodePat Tag [Name]
  | LitPat  SimpleValue
  | DefaultPat
  deriving (Eq, Show, Ord)

-- * Bind Pattern

-- | Bind patterns that can be found in the EBind epxressions.
data BPat
  = BNodePat Tag [Name]
  | BVar     Name
  | BUnit
  deriving (Eq, Show, Ord)

externals :: Exp -> [External]
externals = \case
  Program es _ -> es
  _            -> []

-- * Programs

programToDefs :: Program -> Map Name Exp
programToDefs = \case
  (Program _ defs) -> fromList ((\d@(Def n _ _) -> (n,d)) <$> defs)
  _                -> mempty


-- * Template Haskell

makeBaseFunctor ''Exp

deriving instance Show a  => Show (ExpF a)
deriving instance Eq a    => Eq   (ExpF a)
deriving instance Ord a   => Ord  (ExpF a)

-- * Pretty

instance Pretty Exp where
  pretty = prettyProgram WithExternals

instance Pretty CPat where
  pretty = \case
    NodePat tag vars  -> parens $ hsep (pretty tag : fmap pretty vars)
    LitPat  lit       -> pretty lit
    DefaultPat        -> keyword "#default"

instance Pretty BPat where
  pretty = \case
    BNodePat tag args -> parens $ hsep (pretty tag : fmap pretty args)
    BVar name         -> pretty name
    BUnit             -> parens Grin.Pretty.empty

prettyExternals :: [External] -> Doc
prettyExternals exts = vcat (map prettyExt exts) where
  prettyExt External{..} = prettyFunction (eName, (eRetType, eArgsType))

instance Pretty Ty where
  pretty = \case
    TyCon name tys      -> braces . hsep $ (green $ pretty name) : fmap pretty tys
    TyVar name          -> text "%" <> cyan (pretty name)
    TySimple st         -> pretty st

data RenderingOption
  = Simple
  | WithExternals
  deriving (Eq, Ord, Show, Read)

prettyProgram :: RenderingOption -> Exp -> Doc
prettyProgram Simple          (Program exts e) = prettyHighlightExternals exts (Program [] e)
prettyProgram WithExternals p@(Program exts _) = prettyHighlightExternals exts p
prettyProgram _             p                  = prettyHighlightExternals [] p

-- | Print a given expression with highlighted external functions.
prettyHighlightExternals :: [External] -> Exp -> Doc
prettyHighlightExternals exts = cata folder where
  folder = \case
    ProgramF es defs  -> vcat (prettyExternals es : defs)
    DefF name args exp  -> hsep (pretty name : fmap pretty args) <+> text "=" <$$> indent 2 exp <> line
    -- Exp
    EBindF simpleexp BUnit exp -> simpleexp <$$> exp
    EBindF simpleexp lpat  exp -> pretty lpat <+> text "<-" <+> simpleexp <$$> exp
    ECaseF val alts   -> keyword "case" <+> pretty val <+> keyword "of" <$$> indent 2 (vsep alts)
    -- Simple Expr
    SAppF name args         -> hsep (((if isExternalName exts name then dullyellow else cyan) $ pretty name) : text "$" : fmap pretty args)
    SPureF val              -> keyword "pure" <+> pretty val
    SStoreF val             -> keywordR "store" <+> pretty val
    SFetchF  name           -> keywordR "fetch" <+> pretty name
    SUpdateF name val       -> keywordR "update" <+> pretty name <+> pretty val
    -- Alt
    AltF cpat exp     -> pretty cpat <+> text "->" <$$> indent 2 exp
    -- Block
    BlockF exp              -> text "do" <$$> indent 2 exp

printGrin :: Exp -> IO ()
printGrin = putStrLn . showWide . pretty
