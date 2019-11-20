{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, InstanceSigs, LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, RecordWildCards #-}
module Grin.Interpreter.Abstract.Base where

import Control.Applicative (Alternative(..))
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logic hiding (fail)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.RWS.Strict hiding (ask, local, get)
import Grin.Exp(Exp(..), Program, programToDefs)
import Grin.Value (Name, Tag)
import Grin.Pretty hiding (SChar)
import Lens.Micro.Platform
import Prelude hiding (fail)

import Grin.Interpreter.Store (Store)
import Grin.TypeEnv.Intermediate
import qualified Grin.Interpreter.Store as Store
import Grin.Interpreter.Env (Env(..))
import qualified Grin.Interpreter.Env as Env
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set; import Data.Set (Set)
import qualified Grin.Value as Grin

-- | The Abstract environment consist of externals, functions defined in the program
-- and an Environment which can vary.
data AbsEnv
  = AbsEnv
  { _absExt :: Map.Map Name (T, [T])  -- External functions with their expected type. Return type and types of argument.
  , _absEnv :: Env T                  -- Names in scope
  , _absFun :: Map.Map Name Exp -- How to record the type of function parameters? As now the interpeter calls the
                                -- function during the evaluation, and the current parameters are recorded in the
                                -- environment? Maybe a state will be needed instead of the Reader monad?
                                -- Or the lookupFun and the SApp needs a different implementation? OR the SApp
                                -- should be implemented as the ecase?
  }

makeLenses ''AbsEnv

-- | The abstract store associates a set of possible node types
-- with the abstract location.
type AbsStore = Store Loc (Set Node)

-- | The abstract state consists only the Abstract Store.
newtype AbsState
  = AbsState
  { _absStr :: AbsStore
  } deriving (Show, Eq)

makeLenses ''AbsState


-- Exercise: Read the https://plum-umd.github.io/abstracting-definitional-interpreters/#%28part._s~3acache%29
-- to get the high level idea of the definition below.
newtype AbstractT m a = AbstractT
  { abstractT ::
      RWST -- Reader Writer State of the current execution stack
        AbsEnv    -- Abstract environment for the current execution
        ()        -- No logging
        AbsState  -- Abstract Store for the current execution stack
        (LogicT               -- LogicT for handle non-determinism from branching in case expressions, serves as the Input Output Cache
          (RWST               -- Reader Writer State for the whole program, where the
            (TypeEnv, Cache)  --  Reader is an input parameter for the fix-point finding in one iteration
            ()                --  No Logging
            (TypeEnv, Cache)  --  State is the output for the fix-point finding in one iteration
            m))
        a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState AbsState
    , MonadReader AbsEnv
    , Alternative
    , MonadPlus
    )

-- | Pattern match failure result in backtracking, maintaning the learnt information.
instance MonadFail (AbstractT m) where
  fail _ = mzero

-- | How to run the AbstractT monad-stack
runAbstractT
  :: (Monad m, MonadFail m, MonadIO m)
  => Program -> [(Name, (T, [T]))]
  -> AbstractT m a -> m ([(a, AbsState, ())], (TypeEnv,Cache), ())
runAbstractT prog ops m =
  runRWST
    (observeAllT
      (runRWST
        (abstractT m)
        (AbsEnv (Map.fromList ops) Env.empty (programToDefs prog))
        (AbsState Store.empty)))
    mempty
    mempty

-- | Simple operator to handle branching.
forMonadPlus :: (MonadPlus m) => [a] -> (a -> m b) -> m b
forMonadPlus xs k = msum (map k xs)

-- As mentioned in the article, abstract interpretation must use
-- some kind of caching of the already visited configurations.
-- If a configuration visited once, than it will be part of the
-- cache. When the abstract interpretation tries to revisit the
-- same configuration it will return the already computed result
-- from the cache.

-- The configuration for GRIN consist of a function call
data CExp = CApp Name [Name]
  deriving (Eq, Show, Ord)

-- | The visited configuration
data Config = Config
  { cfgExp    :: CExp     -- The function which was called
  , cfgStore  :: AbsStore -- The abstract store in which this configuration is called.
  , cfgEnv    :: Env T
  } deriving (Eq, Show, Ord)

-- | The cache associates a function call
-- with a set of the type of the return value and the state of the abstract store.
data Cache = Cache (Map.Map Config (Set.Set (T, AbsStore)))
  deriving (Eq, Show)


-- * Cache operations

inCache :: Config -> Cache -> Bool
inCache c (Cache m) = Map.member c m

getCache :: Config -> Cache -> [(T, AbsStore)]
getCache c (Cache m) = maybe [] Set.toList $ Map.lookup c m

insertCache :: Config -> [(T, AbsStore)] -> Cache -> Cache
insertCache c vos (Cache m) = Cache (Map.unionWith (<>) m (Map.singleton c (Set.fromList vos)))

-- * Monoid instances

instance Semigroup TypeEnv where
  TypeEnv l1 v1 f1 <> TypeEnv l2 v2 f2 = TypeEnv (l1 <> l2) (v1 <> v2) (Map.unionWith (<>) f1 f2)

instance Monoid TypeEnv where
  mempty = TypeEnv mempty mempty mempty

instance Semigroup Cache where
  (Cache ma) <> (Cache mb) = Cache (Map.unionWith (Set.union) ma mb)

instance Monoid Cache where
  mempty = Cache mempty

instance Semigroup FunctionT where
  (FunctionT (r1,ps1)) <> (FunctionT (r2,ps2)) = FunctionT ((r1 <> r2),(zipWith (<>) ps1 ps2)) -- TODO: Check

instance Monoid FunctionT where
  mempty = FunctionT mempty
