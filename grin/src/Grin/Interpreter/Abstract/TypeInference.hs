{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, InstanceSigs, LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, RecordWildCards #-}
module Grin.Interpreter.Abstract.TypeInference where

import Control.Applicative (Alternative(..))
import Control.Monad (forM_, msum)
import Control.Monad (when)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logic hiding (fail)
import Data.Function (fix)
import Data.Maybe (fromJust)
import Data.Maybe (isNothing)
import Grin.Exp (Program, eName, externals, Exp(SApp))
import Grin.GExpToExp (gexpToExp)
import Grin.Interpreter.Abstract.Base
import Grin.Interpreter.Abstract.Interpreter
import Grin.Interpreter.Base (baseEval)
import Grin.Interpreter.Env (Env(..))
import Grin.Interpreter.Store (Store(..))
import Grin.Pretty hiding (SChar)
import Grin.TypeEnv.Convert
import Grin.TypeEnv.Intermediate
import Grin.TypeEnv.Result hiding (TypeEnv(..), Loc)
import Grin.Value (Name, SimpleValue)
import Prelude hiding (fail)

import qualified Data.List as List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set; import Data.Set (Set)
import qualified Grin.Examples as Examples
import qualified Grin.TypeEnv.Result as Grin



-- * Tests

tests :: IO ()
tests = do
  print =<< (PP <$> (typeInference $ gexpToExp $ Examples.add))
  print =<< (PP <$> (typeInference $ gexpToExp $ Examples.fact))
  print =<< (PP <$> (typeInference $ gexpToExp $ Examples.sumSimple))

typeInference :: (Monad m, MonadFail m, MonadIO m) => Program -> m Grin.TypeEnv
typeInference = fmap (calcTypeEnv . fst) . abstractEval

abstractEval :: (Monad m, MonadFail m, MonadIO m) => Program -> m (TypeEnv, Cache)
abstractEval prog = do
  let ops = [ ("prim_int_add", prim_int_add)
            , ("prim_int_sub", prim_int_sub)
            , ("prim_int_mul", prim_int_mul)
            , ("prim_int_print", prim_int_print)
            , ("prim_int_eq", prim_int_eq)
            , ("prim_int_gt", prim_int_gt)
            ]
  let opsMap = Map.fromList ops
  forM_ exts $ \ext -> do
    when (isNothing (Map.lookup (eName ext) opsMap)) $
      fail $ "Missing external: " ++ show (eName ext)
  (\(_,tc,_) -> tc) <$> runAbstractT prog ops (fixCache (fix (evalCache baseEval)) (SApp "main" []))
  where
    exts = externals prog
    prim_int_add    = (ST ST_Int64, [ST ST_Int64, ST ST_Int64])
    prim_int_sub    = (ST ST_Int64, [ST ST_Int64, ST ST_Int64])
    prim_int_mul    = (ST ST_Int64, [ST ST_Int64, ST ST_Int64])
    prim_int_eq     = (ST ST_Bool,  [ST ST_Int64, ST ST_Int64])
    prim_int_gt     = (ST ST_Bool,  [ST ST_Int64, ST ST_Int64])
    prim_int_print  = (UT, [ST ST_Int64])
