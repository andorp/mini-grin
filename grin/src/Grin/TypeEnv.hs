{-# LANGUAGE LambdaCase, TemplateHaskell, RecordWildCards #-}
module Grin.TypeEnv where

import Prelude hiding (exp)

import Data.Function (on)
import Data.List (sortBy)
import Data.Map
import Grin.Pretty
import Grin.Value
import Lens.Micro.Platform
import qualified Data.Map.Strict as Map



type Loc = Int

data SimpleType
  = T_Int64
  | T_Word64
  | T_Float
  | T_Bool
  | T_Char

  | T_Location {_locations :: [Loc]}
  | T_Unit
  deriving (Eq, Ord, Show)

type NodeSet = Map Tag [SimpleType]

data Type
  = T_SimpleType  {_simpleType  :: SimpleType}
  | T_NodeSet     {_nodeSet     :: NodeSet}
  deriving (Eq, Ord, Show)

data TypeEnv
  = TypeEnv
  { _location :: Map Int NodeSet
  , _variable :: Map Name Type
  , _function :: Map Name (Type, [Type])
  }
  deriving (Eq, Show)

data Ty
  = TyCon     Name [Ty]
  | TyVar     Name
  | TySimple  SimpleType
  deriving (Eq, Ord, Show)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv mempty mempty mempty

-- * Template Haskell

makeLenses ''TypeEnv
makeLenses ''Type
makeLenses ''SimpleType

instance Pretty SimpleType where
  pretty = \case
    T_Location l -> encloseSep lbrace rbrace comma $ fmap (cyan . int) l
    ty -> red $ text $ show ty

prettyNode :: (Tag, [SimpleType]) -> Doc
prettyNode (tag, args) = pretty tag <> list (fmap pretty args)

instance Pretty Type where
  pretty = \case
    T_SimpleType ty -> pretty ty
    T_NodeSet ns    -> encloseSep lbrace rbrace comma (fmap prettyNode (Map.toList ns))

instance Pretty TypeEnv where
  pretty TypeEnv{..} = vsep
    [ yellow (text "Location") <$$> indent 4 (prettyKeyValue $ sortBy (compare `on` fst) $ Map.toList $ Map.map T_NodeSet _location)
    , yellow (text "Variable") <$$> indent 4 (prettyKeyValue $ Map.toList _variable)
    , yellow (text "Function") <$$> indent 4 (vsep $ fmap prettyFunction $ Map.toList _function)
    ]

