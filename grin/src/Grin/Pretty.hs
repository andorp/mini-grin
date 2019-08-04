{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings #-}
module Grin.Pretty
  ( pretty
  , printGrin
  , PP(..)
  , WPP(..)
  , RenderingOption(..)
  , prettyProgram
  , prettyHighlightExternals
  , prettyKeyValue
  , prettyBracedList
  , prettySimplePair
  , prettyFunction
  , Pretty
  , showWidth
  , showWide
  ) where

import Data.Char
import Data.Set (Set)
import Data.List (groupBy)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Text (unpack)

import Data.Functor.Foldable as Foldable
import Text.PrettyPrint.ANSI.Leijen

import Grin.Exp
import Data.Function (on)
import Data.List (sortBy)


showWidth :: Int -> Doc -> String
showWidth w x = displayS (renderPretty 0.4 w x) ""

showWide :: Doc -> String
showWide = showWidth 156

printGrin :: Exp -> IO ()
printGrin = putStrLn . showWide . pretty

-- plain wrappers ; remove colors

-- Pretty Show instance wrapper ; i.e. useful for hspec tests
newtype PP a = PP a deriving Eq
instance Pretty a => Show (PP a ) where
  show (PP a) = showWide . plain . pretty $ a

-- Wide pretty printing, useful for reparsing pretty-printed ASTs
newtype WPP a = WPP a deriving Eq
instance Pretty a => Show (WPP a) where
  show (WPP a) = showWide . plain . pretty $ a


keyword :: String -> Doc
keyword = yellow . text

keywordR = red . text

instance Pretty Name where
  pretty = text . nameString

data RenderingOption
  = Simple
  | WithExternals
  deriving (Eq, Ord, Show, Read)

prettyProgram :: RenderingOption -> Exp -> Doc
prettyProgram Simple          (Program exts e) = prettyHighlightExternals exts (Program [] e)
prettyProgram WithExternals p@(Program exts _) = prettyHighlightExternals exts p
prettyProgram _             p                  = prettyHighlightExternals [] p

-- TODO
--  nice colors for syntax highlight
--  better node type syntax (C | F | P)

-- | Print a given expression with highlighted external functions.
prettyHighlightExternals :: [External] -> Exp -> Doc
prettyHighlightExternals externals exp = cata folder exp where
  folder = \case
    ProgramF exts defs  -> vcat (prettyExternals exts : defs)
    DefF name args exp  -> hsep (pretty name : map pretty args) <+> text "=" <$$> indent 2 exp <> line
    -- Exp
    EBindF simpleexp Unit exp -> simpleexp <$$> exp
    EBindF simpleexp lpat exp -> pretty lpat <+> text "<-" <+> simpleexp <$$> exp
    ECaseF val alts   -> keyword "case" <+> pretty val <+> keyword "of" <$$> indent 2 (vsep alts)
    -- Simple Expr
    SAppF name args         -> hsep (((if isExternalName externals name then dullyellow else cyan) $ pretty name) : text "$" : map pretty args)
    SPureF val              -> keyword "pure" <+> pretty val
    SStoreF val             -> keywordR "store" <+> pretty val
    SFetchF  name           -> keywordR "fetch" <+> pretty name
    SUpdateF name val       -> keywordR "update" <+> pretty name <+> pretty val
    -- Alt
    AltF cpat exp     -> pretty cpat <+> text "->" <$$> indent 2 exp


instance Pretty Exp where
  pretty = prettyProgram Simple

instance Pretty Val where
  pretty = \case
    ConstTagNode tag args -> parens $ hsep (pretty tag : map pretty args)
    Unit         -> parens empty
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

instance Pretty CPat where
  pretty = \case
    NodePat tag vars  -> parens $ hsep (pretty tag : map pretty vars)
    LitPat  lit       -> pretty lit
    DefaultPat        -> keyword "#default"

instance Pretty TagType where
  pretty = green . \case
    C   -> text "C"
    F   -> text "F"
    P i -> text "P" <> int i

instance Pretty Tag where
  pretty (Tag tagtype name) = pretty tagtype <> pretty name

-- generic ; used by HPTResult and TypeEnv

instance Pretty a => Pretty (Set a) where
  pretty s = encloseSep lbrace rbrace comma (map pretty $ Set.toList s)

prettyKeyValue :: (Pretty k, Pretty v) => [(k,v)] -> Doc
prettyKeyValue kvList = vsep [fill 6 (pretty k) <+> text "->" <+> pretty v | (k,v) <- kvList]

-- type env

instance Pretty SimpleType where
  pretty = \case
    T_Location l -> encloseSep lbrace rbrace comma $ map (cyan . int) l
    ty -> red $ text $ show ty

prettyNode :: (Tag, [SimpleType]) -> Doc
prettyNode (tag, args) = pretty tag <> list (map pretty args)

instance Pretty Type where
  pretty = \case
    T_SimpleType ty -> pretty ty
    T_NodeSet ns    -> encloseSep lbrace rbrace comma (map prettyNode (Map.toList ns))

instance Pretty TypeEnv where
  pretty TypeEnv{..} = vsep
    [ yellow (text "Location") <$$> indent 4 (prettyKeyValue $ sortBy (compare `on` fst) $ Map.toList $ Map.map T_NodeSet _location)
    , yellow (text "Variable") <$$> indent 4 (prettyKeyValue $ Map.toList _variable)
    , yellow (text "Function") <$$> indent 4 (vsep $ map prettyFunction $ Map.toList _function)
    ]


prettyExternals :: [External] -> Doc
prettyExternals exts = vcat (map prettyExtGroup $ groupBy (\a b -> eEffectful a == eEffectful b && eKind a == eKind b) exts) where
  prettyExtGroup [] = mempty
  prettyExtGroup l@(a : _) = keyword "primop" <+> (if eEffectful a then keyword "effectful" else keyword "pure") <$$> indent 2
    (vsep [prettyFunction (eName, (eRetType, eArgsType)) | External{..} <- l] <> line)

instance Pretty Ty where
  pretty = \case
    TyCon name tys      -> braces . hsep $ (green $ pretty name) : map pretty tys
    TyVar name          -> text "%" <> cyan (pretty name)
    TySimple simpleType -> pretty simpleType

prettyBracedList :: [Doc] -> Doc
prettyBracedList = encloseSep lbrace rbrace comma

prettySimplePair :: (Pretty a, Pretty b) => (a, b) -> Doc
prettySimplePair (x, y) = pretty x <> pretty y

prettyFunction :: (Pretty a, Pretty name) => (name, (a, [a])) -> Doc
prettyFunction (name, (ret, args)) = pretty name <> align (encloseSep (text " :: ") empty (text " -> ") (map pretty $ args ++ [ret]))

prettyLocSet :: Set Loc -> Doc
prettyLocSet = semiBraces . map (cyan . int) . Set.toList
