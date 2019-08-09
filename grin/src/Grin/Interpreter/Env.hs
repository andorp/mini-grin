{-# LANGUAGE DeriveFunctor #-}
module Grin.Interpreter.Env where

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Grin.Exp hiding (Val)
import Grin.Pretty
import qualified Data.Map.Strict as Map

-- * Env

newtype Env v = Env (Map.Map Name v)
  deriving (Eq, Show, Ord, Functor)

emptyEnv :: Env v
emptyEnv = Env mempty

lookupEnv :: (Env v) -> Name -> v
lookupEnv (Env m) n = fromMaybe (error $ "Missing:" ++ show n) $ Map.lookup n m

extendEnv :: Env v -> [(Name, v)] -> Env v
extendEnv (Env m) vs = Env $ foldl' (\n (k,v) -> Map.insert k v n) m vs

instance (Semigroup v) => Semigroup (Env v) where
  Env m1 <> Env m2 = Env (Map.unionWith (<>) m1 m2)

instance (Semigroup v) => Monoid (Env v) where
  mempty = Env mempty

instance (Pretty v) => Pretty (Env v) where
  pretty (Env m) = prettyKeyValue (Map.toList m)
