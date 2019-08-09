module Grin.Interpreter.Store where

import Data.Maybe (fromMaybe)
import Grin.Pretty
import qualified Data.Map.Strict as Map

-- * Store

newtype Store a v = Store (Map.Map a v)
  deriving (Eq, Ord, Show)

emptyStore :: (Ord a) => Store a v
emptyStore = Store mempty

storeFind :: (Ord a) => Store a v -> a -> v
storeFind (Store m) a = fromMaybe (error "Store; missing") $ Map.lookup a m

storeExt :: (Ord a) => a -> v -> Store a v -> Store a v
storeExt a v (Store m) = Store (Map.insert a v m)

instance (Ord a, Semigroup v) => Semigroup (Store a v) where
  (Store ma) <> (Store mb) = Store (Map.unionWith (<>) ma mb)

instance (Ord a, Monoid v) => Monoid (Store a v) where
  mempty = Store mempty

instance (Pretty a, Pretty v) => Pretty (Store a v) where
  pretty (Store m) = prettyKeyValue (Map.toList m)
