{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass, DeriveFunctor, TypeFamilies #-}
{-# LANGUAGE DeriveFoldable, DeriveTraversable, PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell, StandaloneDeriving, OverloadedStrings #-}
{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Grin.Exp
  ( module Grin.Exp
  ) where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.List (groupBy)
import Data.Text (Text)
import Grin.Pretty
import Grin.TypeEnv
import Grin.Value
import Lens.Micro.Platform
import Prelude hiding (exp)

-- TODO: Comment on what is the noise in this module
-- TODO: Move the Exp definition to the top

-- * GRIN Externals, i.e. primops and foreign function
data ExternalKind
  = PrimOp -- ^ Implemented in the internal code generator
  | FFI    -- ^ Implemented in C and linked during the linker phase
  deriving (Eq, Ord, Show)

data External
  = External
  { eName       :: Name
  , eRetType    :: Ty
  , eArgsType   :: [Ty]
  , eEffectful  :: Bool
  , eKind       :: ExternalKind
  }
  deriving (Eq, Ord, Show)

isExternalName :: [External] -> Name -> Bool
isExternalName es n = n `Prelude.elem` (eName <$> es)



-- * Case Pattern

data CPat
  = NodePat Tag [Name]
  | LitPat  Lit
  | DefaultPat
  deriving (Eq, Show, Ord)

-- * Bind Pattern

data BPat
  = BNodePat Tag [Name]
  | BVar     Name
  | BUnit
  deriving (Eq, Show, Ord)

-- * GRIN Expression

-- TODO:
type SimpleExp = Exp
type Alt = Exp
type Def = Exp
type Program = Exp

data Exp
  = Program     [External] [Def]
  | Def         Name [Name] Exp
  -- Exp
  | EBind       SimpleExp BPat Exp
  | ECase       Name [Alt]
  -- Simple Exp
  | SApp        Name [Name]
  | SPure       Val
  | SStore      Name -- Variable should hold only nodes
  | SFetch      Name -- Variable should hold only locations
  | SUpdate     Name Name -- The variables in order should hold only location and node
  -- Alt
  | Alt CPat Exp
  deriving (Eq, Ord, Show)

externals :: Exp -> [External]
externals = \case
  Program es _ -> es
  _            -> []

-- * Template Haskell

makeBaseFunctor ''Val
makeBaseFunctor ''Exp

deriving instance Show a  => Show (ExpF a)
deriving instance Eq a    => Eq   (ExpF a)
deriving instance Ord a   => Ord  (ExpF a)

-- * Traversals

_AltCPat :: Traversal' Exp CPat
_AltCPat f (Alt p e) = (`Alt` e) <$> f p
_AltCPat _ other     = pure other

_AltFCPat :: Traversal' (ExpF a) CPat
_AltFCPat f (AltF p e) = (`AltF` e) <$> f p
_AltFCPat _ other      = pure other

_CPatNodeTag :: Traversal' CPat Tag
_CPatNodeTag f (NodePat tag args) = (`NodePat` args) <$> f tag
_CPatNodeTag _ other              = pure other

_CPatLit :: Traversal' CPat Lit
_CPatLit f (LitPat lit) = LitPat <$> f lit
_CPatLit _ other        = pure other

_CPatDefault :: Traversal' CPat ()
_CPatDefault f DefaultPat = const DefaultPat <$> f ()
_CPatDefault _ other      = pure other

_ValVar :: Traversal' Val Name
_ValVar f (Var name) = Var <$> f name
_ValVar _ other      = pure other

_NM :: Traversal' Name Text
_NM f (NM n) = NM <$> f n

-- * Pretty

instance Pretty Exp where
  pretty = prettyProgram Simple

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
prettyExternals exts = vcat (fmap prettyExtGroup $ groupBy (\a b -> eEffectful a == eEffectful b && eKind a == eKind b) exts) where
  prettyExtGroup [] = mempty
  prettyExtGroup l@(a : _) = keyword "primop" <+> (if eEffectful a then keyword "effectful" else keyword "pure") <$$> indent 2
    (vsep [prettyFunction (eName, (eRetType, eArgsType)) | External{..} <- l] <> line)

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

printGrin :: Exp -> IO ()
printGrin = putStrLn . showWide . pretty
