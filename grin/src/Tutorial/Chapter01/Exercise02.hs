{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}
module Tutorial.Chapter01.Exercise02 where

import Data.Int
import Data.Word
import Data.Maybe
import Grin.Exp
import Grin.Interpreter.Env
import Grin.Interpreter.Store
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.RWS.Strict (RWST(..))
import qualified Grin.Value as Grin
import qualified Data.Map.Strict as Map

{-
TODO: Description of the exercise.
-}

data Context = Context
  { externalCall :: Grin.Name -> [Value] -> IO Value
  , functions    :: Map.Map Grin.Name Exp
  }

type Address = Int
newtype Interpreter m a = Interpreter (RWST (Env Value) () (Store Address Node) m a)
  deriving  ( Functor
            , Applicative
            , Monad
            , MonadIO
            , MonadReader (Env Value)
            , MonadState (Store Address Node)
            , MonadFail
            )

runInterpreter
  :: (Monad m, MonadIO m)
  => Interpreter m a -> m (a, Store Address Node)
runInterpreter (Interpreter r) = do
  (a,store,()) <- runRWST r emptyEnv emptyStore
  pure (a,store)

type InterpretExternal = Grin.Name -> [Value] -> IO Value

interpret :: InterpretExternal -> Program -> IO Value
interpret ietx prog =
  fst <$> runInterpreter (eval (Context ietx (programToDefs prog)) (grinMain prog))

data SValue
  = SInt64  Int64
  | SWord64 Word64
  | SFloat  Float
  | SBool   Bool
  | SString String
  | SChar   Char
  | SLoc    Address
  deriving (Eq, Ord, Show)

data Node = N { tag :: Grin.Tag, fields :: [SValue] }
  deriving (Eq, Show)

data Value       -- A runtime value can be:
  = Prim SValue  -- A primitive value aks simple value
  | Node Node    -- A node value which represent a node in the graph
  | Unit         -- The UNIT value, which represents no information at all. Like () in Haskell.
  deriving (Eq, Show)

simpleValue :: Grin.SimpleValue -> SValue
simpleValue = \case
  Grin.SInt64  s -> SInt64  s
  Grin.SWord64 s -> SWord64 s
  Grin.SFloat  s -> SFloat  s
  Grin.SBool   s -> SBool   s
  Grin.SString s -> SString s
  Grin.SChar   s -> SChar   s

valueOf :: Grin.Name -> Interpreter IO Value
valueOf name = asks (`lookupEnv` name)

svalueOf :: Grin.Name -> Interpreter IO SValue
svalueOf name = do
  (Prim sv) <- valueOf name
  pure sv

alloc :: Interpreter IO Address
alloc = gets storeSize

-- Write a function that interprets the given expression. The function gets the body of the main.
eval :: Context -> Exp -> Interpreter IO Value
eval ctx = \case

  -- Convert a node literal value to the grin interpreter value
  SPure (Grin.Lit l) -> case l of
    Grin.LVal  sv               -> pure $ Prim $ simpleValue sv
    Grin.LNode (Grin.Node t ps) -> Node . N t <$> mapM svalueOf ps

  -- Lookup a variable in the environment and return its value
  SPure (Grin.Var n) -> valueOf n

  -- Create a memory location on the heap and store the value which the variable v has.
  SStore n -> do
    (Node node) <- valueOf n
    addr <- alloc
    modify (storeExt addr node)
    pure $ Prim $ SLoc addr

  -- Fetch a value from the heap, addressed by
  -- the memory location stored in the variable h.
  SFetch h -> error "TODO"

  -- Update the value of the memory location h
  -- with the stored value n.
  SUpdate h n -> error "TODO"

  -- Evaluate the left hand side, ignore the value,
  -- and evaluate the right hand side.
  EBind lhs BUnit rhs -> do
    void $ eval ctx lhs
    eval ctx rhs

  -- Evaluate the left hand side, bind its value to the variable x
  -- extending the environment, then evaluate the right hand side
  -- Corresponds to the bind: lhs >>= \pattern -> rhs
  EBind lhs (BVar x) rhs -> error "TODO"

  -- Evaluate the left hand side, bind its value to the pattern
  -- if the node value match the given pattern, otherwise the behaviour
  -- is undefined.
  EBind lhs (BNodePat t xs) rhs -> error "TODO"

  -- Evaluate the variable x and select the the matching alternatives to the
  -- to the value, similar how the pattern matching happened in the EBind,
  -- use your intuition.
  ECase x alts -> do
    v <- valueOf x
    let selectedAlt = (error "TODO") v alts
    eval ctx selectedAlt

  -- Ignore the pattern in the Alt and evaluate the body
  Alt _apat body -> eval ctx body

  -- Call a function. Lookup the function which can be external or internal
  -- bind the values to the function parameters, or pass them to the
  -- interpreter of the externals
  SApp fn callParams -> do
    callValues <- mapM valueOf callParams
    case Map.lookup fn (functions ctx) of
      Nothing -> liftIO $ externalCalls fn callValues
      Just (Def _ funParamNames body) -> do
        -- Creates a new local environment which contains only the function parameters
        local (const $ extendEnv emptyEnv (funParamNames `zip` callValues))
              (eval ctx body)
      overGenerative -> error $ show overGenerative

  overGenerative -> error $ show overGenerative

-- * Helpers

grinMain :: Program -> Exp
grinMain = \case
  (Program _ defs) -> head $ flip mapMaybe defs $ \case
                        (Def n _ b) -> if n == "main" then Just b else Nothing
                        _           -> Nothing
  _                -> error "grinMain"

programToDefs :: Program -> Map.Map Grin.Name Exp
programToDefs = \case
  (Program _ defs) -> Map.fromList ((\d@(Def n _ _) -> (n,d)) <$> defs)
  _                -> mempty

externalCalls :: Grin.Name -> [Value] -> IO Value
externalCalls ext args = case ( ext, args) of
  ("prim_int_eq",  [Prim (SInt64 a), Prim (SInt64 b)]) -> pure $ Prim (SBool (a == b))
  ("prim_int_gt",  [Prim (SInt64 a), Prim (SInt64 b)]) -> pure $ Prim (SBool (a > b))
  ("prim_int_add", [Prim (SInt64 a), Prim (SInt64 b)]) -> pure $ Prim (SInt64 (a + b))
  ("prim_int_sub", [Prim (SInt64 a), Prim (SInt64 b)]) -> pure $ Prim (SInt64 (a - b))
  ("prim_int_mul", [Prim (SInt64 a), Prim (SInt64 b)]) -> pure $ Prim (SInt64 (a * b))
  ("prim_int_print", [val@(Prim (SInt64 a))]) -> Unit <$ print a
  other -> error ("non-existing external, or bad args " <> show other)
