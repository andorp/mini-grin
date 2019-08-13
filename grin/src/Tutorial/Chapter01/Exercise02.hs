{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}
module Tutorial.Chapter01.Exercise02 where

import Data.Maybe
import Grin.Exp
import Grin.Interpreter.Env
import Grin.Interpreter.Store
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.RWS.Strict (RWST(..))
import qualified Grin.Value as Grin
import qualified Data.Map.Strict as Map


data Context = Context
  { externalCall :: External -> [Value] -> IO Value
  , functions    :: Map.Map Grin.Name Exp
  }

-- TODO: Rename this to address
type Address = Int
newtype Interpreter m a = Interpreter (RWST (Env Value) () (Store Address Node) m a)
  deriving  ( Functor
            , Applicative
            , Monad
            , MonadReader (Env Value)
            , MonadState (Store Address Node)
            )

runInterpreter
  :: (Monad m, MonadIO m)
  => Interpreter m a -> m (a, Store Address Node)
runInterpreter (Interpreter r) = do
  (a,store,()) <- runRWST r emptyEnv emptyStore
  pure (a,store)

type InterpretExternal = External -> [Value] -> IO Value


interpret :: InterpretExternal -> Program -> IO Value
interpret ietx prog =
  fst <$> runInterpreter (eval (Context ietx (programToDefs prog)) (grinMain prog))

data PValue
  = Loc Address
  | PV  Grin.Lit
  deriving (Eq, Show)

data Node = N { tag :: Grin.Tag, fields :: [PValue] }
  deriving (Eq, Show)

data Value       -- A runtime value can be:
  = Prim PValue  -- A primitive value
  | Node Node    -- A node value which represent a node in the graph
  | Unit         -- The UNIT value, which represents no information at all. Like () in Haskell.
  deriving (Eq, Show)


-- Write a function that interprets the given expression. The function gets the body of the main.
eval :: Context -> Exp -> Interpreter IO Value
eval ctx = \case

  -- Convert a node literal value to the grin interpreter node value
  SPure n@(Grin.CNode{}) -> literal n

  -- Convert a simple literal value to the grin interpreter simple value
  SPure l@(Grin.Lit{}) -> literal l

  -- Convert a unit literal value to the grin interpreter value
  SPure u@Grin.Unit -> literal u

  -- Lookup a variable in the environment and return its value
  SPure (Grin.Var n) -> error "TODO"

  -- Create a memory location on the heap and store the value which the variable v has.
  SStore v -> error "TODO"

  -- Fetch a value from the heap, addressed by
  -- the memory location stored in the variable h.
  SFetch h -> error "TODO"

  -- Update the value of the memory location h
  -- with the stored value n.
  SUpdate h n -> error "TODO"

  -- Evaluate the left hand side, bind its value to the variable x
  -- extending the environment, then evaluate the right hand side
  EBind lhs (BVar x) rhs -> error "TODO"

  -- Evaluate the left hand side, bind its value to the pattern
  -- if the node value match the given pattern, otherwise the behaviour
  -- is undefined.
  EBind lhs (BNodePat t xs) rhs -> error "TODO"

  -- Evaluate the left hand side, ignore the value,
  -- and evaluate the right hand side.
  EBind lhs BUnit rhs -> error "TODO"

  -- Evaluate the variable x and select the the matching alternatives to the
  -- to the value, similar how the pattern matching happened in the EBind,
  -- use your intuition.
  ECase x alts -> error "TODO"

  -- Ignore the pattern in the Alt and evaluate the body
  Alt _apat body -> error "TODO"

  -- Call a function. Lookup the function which can be external or internal
  -- bind the values to the function parameters, or pass them to the
  -- interpreter of the externals
  SApp fn ps -> error "TODO"

  overGenerative -> error $ show overGenerative

-- The Val and Val should be separated as Literal and Value for the interpreter
literal :: Grin.Val -> Interpreter IO Value
literal _ = error "TODO"

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

externalCalls :: External -> [Value] -> IO Value
externalCalls ext args = case (eName ext, args) of
  ("prim_int_eq",  [Prim (PV (Grin.LInt64 a)), Prim (PV (Grin.LInt64 b))]) -> pure $ Prim (PV (Grin.LBool (a == b)))
  ("prim_int_gt",  [Prim (PV (Grin.LInt64 a)), Prim (PV (Grin.LInt64 b))]) -> pure $ Prim (PV (Grin.LBool (a > b)))
  ("prim_int_add", [Prim (PV (Grin.LInt64 a)), Prim (PV (Grin.LInt64 b))]) -> pure $ Prim (PV (Grin.LInt64 (a + b)))
  ("prim_int_sub", [Prim (PV (Grin.LInt64 a)), Prim (PV (Grin.LInt64 b))]) -> pure $ Prim (PV (Grin.LInt64 (a - b)))
  ("prim_int_mul", [Prim (PV (Grin.LInt64 a)), Prim (PV (Grin.LInt64 b))]) -> pure $ Prim (PV (Grin.LInt64 (a * b)))
  ("prim_int_print", [val@(Prim (PV (Grin.LInt64 a)))]) -> Unit <$ print a
  other -> error ("non-existing external, or bad args " <> show other)
