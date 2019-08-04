{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass, DeriveFunctor, TypeFamilies #-}
{-# LANGUAGE DeriveFoldable, DeriveTraversable, PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell, StandaloneDeriving, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Grin.Exp
  ( module Grin.Exp
  ) where

import Control.DeepSeq
import Data.Binary
import Data.ByteString.Short as B
import Data.Data
import Data.Map
import Data.Functor.Foldable.TH
import Data.Int
import Data.String
import Data.Text (Text, isPrefixOf, unpack)
import Data.Vector
import Data.Word
import GHC.Generics (Generic)
import Lens.Micro.Platform
import Text.Printf


-- Names are stored in NM form when we do program generation. NI is only used
-- when we seralize the Exp
data Name
  = NM { unNM :: !Text }
  | NI !Int
  deriving (Generic, Data, NFData, Eq, Ord, Show)

nMap :: (Text -> Text) -> Name -> Name
nMap f (NM n) = NM (f n)

instance Semigroup  Name where (NM n1) <> (NM n2) = NM (n1 <> n2)
instance Monoid     Name where mempty = NM mempty
instance IsString   Name where fromString = NM . fromString
instance PrintfArg  Name where formatArg = formatString . Data.Text.unpack . unNM

nameString :: Name -> String
nameString = \case
  NM n -> Data.Text.unpack n
  _    -> error "Name index found." -- This could have left in the AST after a problematic deserialisation.

-- * GRIN Tag

data TagType = C | F | P Int {-missing parameter count-}
  deriving (Generic, Data, NFData, Eq, Ord, Show)

data Tag = Tag { tagType :: TagType, tagName :: Name }
  deriving (Generic, Data, NFData, Eq, Ord, Show)

-- * GRIN Externals, i.e. primops and foreign functions

data Ty
  = TyCon     Name [Ty]
  | TyVar     Name
  | TySimple  SimpleType
  deriving (Generic, Data, NFData, Eq, Ord, Show)

data ExternalKind
  = PrimOp -- ^ Implemented in the internal code generator
  | FFI    -- ^ Implemented in C and linked during the linker phase
  deriving (Generic, Data, NFData, Eq, Ord, Show)

data External
  = External
  { eName       :: Name
  , eRetType    :: Ty
  , eArgsType   :: [Ty]
  , eEffectful  :: Bool
  , eKind       :: ExternalKind
  }
  deriving (Generic, Data, NFData, Eq, Ord, Show)

isExternalName :: [External] -> Name -> Bool
isExternalName es n = n `Prelude.elem` (eName <$> es)

-- * GRIN Literal

data Lit
  = LInt64  Int64
  | LWord64 Word64
  | LFloat  Float
  | LBool   Bool
  | LString Text
  | LChar   Char
  deriving (Generic, Data, NFData, Eq, Ord, Show)

-- * GRIN Value

type LPat = Val -- ConstTagNode, VarTagNode, ValTag, Unit, Lit, Var
type SimpleVal = Val

data Val
  = ConstTagNode  Tag  [Name]       -- complete node (constant tag) ; HIGH level GRIN
  | Unit                            -- HIGH level GRIN
  -- simple val
  | Lit Lit                         -- HIGH level GRIN
  | Var Name                        -- HIGH level GRIN
  deriving (Generic, Data, NFData, Eq, Ord, Show)

-- * Case Pattern

data CPat
  = NodePat Tag [Name]  -- HIGH level GRIN
  | LitPat  Lit         -- HIGH level GRIN
  | DefaultPat          -- HIGH level GRIN
  deriving (Generic, Data, NFData, Eq, Show, Ord)

-- * GRIN Expression

type SimpleExp = Exp
type Alt = Exp
type Def = Exp
type Program = Exp

data Exp
  = Program     [External] [Def]
  | Def         Name [Name] Exp
  -- Exp
  | EBind       SimpleExp LPat Exp
  | ECase       Name [Alt]
  -- Simple Exp
  | SApp        Name [Name]
  | SPure       Val
  | SStore      Name -- Variable should hold only nodes
  | SFetch      Name -- Variable should hold only locations
  | SUpdate     Name Name -- The variables in order should hold only location and node
  -- Alt
  | Alt CPat Exp
  deriving (Generic, Data, NFData, Eq, Ord, Show)

externals :: Exp -> [External]
externals = \case
  Program es _ -> es
  _            -> []

-- * GRIN Type System

type Loc = Int

data SimpleType
  = T_Int64
  | T_Word64
  | T_Float
  | T_Bool
  | T_Unit
  | T_Location {_locations :: [Loc]}
  | T_String
  | T_Char
  deriving (Generic, Data, NFData, Eq, Ord, Show)

type NodeSet = Map Tag [SimpleType]

data Type
  = T_SimpleType  {_simpleType  :: SimpleType}
  | T_NodeSet     {_nodeSet     :: NodeSet}
  deriving (Generic, Data, NFData, Eq, Ord, Show)

data TypeEnv
  = TypeEnv
  { _location :: Map Int NodeSet
  , _variable :: Map Name Type
  , _function :: Map Name (Type, [Type])
  }
  deriving (Eq, Show)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv mempty mempty mempty


-- * Template Haskell

-- makeLenses ''TypeEnv
makeLenses ''Type
makeLenses ''SimpleType

makeBaseFunctor ''Val
makeBaseFunctor ''Exp

-- * Binary instances

deriving instance Binary Name
deriving instance Binary ExternalKind
deriving instance Binary External
deriving instance Binary Ty
deriving instance Binary Lit
deriving instance Binary Tag
deriving instance Binary CPat
deriving instance Binary Val
deriving instance Binary Exp
deriving instance Binary SimpleType
deriving instance Binary TagType
deriving instance Binary Type

instance Binary a => Binary (Vector a) where
  get = Data.Vector.fromList <$> get
  put = put . Data.Vector.toList

deriving instance Show a  => Show (ExpF a)
deriving instance Eq a    => Eq   (ExpF a)
deriving instance Ord a   => Ord  (ExpF a)

pattern BoolPat b = LitPat (LBool b)

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

_TyCon :: Traversal' Ty (Name, [Ty])
_TyCon f (TyCon n ts) = uncurry TyCon <$> f (n, ts)
_TyCon _ other        = pure other

_TyVar :: Traversal' Ty Name
_TyVar f (TyVar n) = TyVar <$> f n
_TyVar _ other     = pure other

_TySimple :: Traversal' Ty SimpleType
_TySimple f (TySimple t) = TySimple <$> f t
_TySimple _ other        = pure other

_NM :: Traversal' Name Text
_NM f (NM n) = NM <$> f n
_NM _ other  = pure other

_NI :: Traversal' Name Int
_NI f (NI i) = NI <$> f i
_NI _ other  = pure other
