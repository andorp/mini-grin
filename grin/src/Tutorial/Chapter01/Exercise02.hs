{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}
module Tutorial.Chapter01.Exercise02 where

import Data.Maybe
import Grin.Exp
import Grin.Value
import Grin.Interpreter.Env
import Grin.Interpreter.Store
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.RWS.Strict
import qualified Data.Map.Strict as Map


data Context = Context
  { externalCall :: External -> [Val] -> IO Val
  , funcions     :: Map.Map Name Exp
  }

newtype Heap = Heap Int
newtype Interpreter m a = Interpreter (RWST (Env Val) () (Store Int Node) m a)
  deriving  ( Functor
            , Applicative
            , Monad
            , MonadReader (Env Val)
            , MonadState (Store Int Node)
            )

runInterpreter
  :: (Monad m, MonadIO m)
  => Interpreter m a -> m (a, Store Int Node)
runInterpreter (Interpreter r) = do
  (a,store,()) <- runRWST r emptyEnv emptyStore
  pure (a,store)

type InterpretExternal = External -> [Val] -> IO Val

-- Implement the external calls for the externals used in the Examples
externalCalls :: External -> [Val] -> IO Val
externalCalls = undefined

todo :: Interpreter m a
todo = error "TODO"


interpret :: InterpretExternal -> Program -> IO Val
interpret ietx prog = fmap fst $ runInterpreter $ eval (Context ietx (programToDefs prog)) (grinMain prog)

-- Write a function that interprets the given expression. The function gets the body of the main.
eval :: Context -> Exp -> Interpreter IO Val
eval ctx = \case

  SPure n@(CNode{}) -> literal n -- Convert a node literal value to the grin interpreter node value
  SPure l@(Lit{})   -> literal l -- Convert a simple literal value to the grin interpreter literal value
  SPure u@Unit      -> literal u -- Convert a unit literal value to the grin interpreter value
  SPure (Var n)     -> todo -- Lookup a variable in the environment and return its value

  SStore v    -> todo -- Create a memory location on the heap and store the value which the variable v has.
  SFetch h    -> todo -- Fetch a value from the heap, addressed by
                      -- the memory location stored in the variable h.
  SUpdate h n -> todo -- Update the value of the memory location h
                      -- with the stored value n.

  EBind lhs (BVar x)        rhs -> todo -- Evaluate the left hand side, bind its value to the variable x
                                        -- extending the environment, then evaluate the right hand side
  EBind lhs (BNodePat t xs) rhs -> todo -- Evaluate the left hand side, bind its value to the pattern
                                        -- if the node value match the given pattern, otherwise the behaviour
                                        -- is undefined.
  EBind lhs BUnit           rhs -> todo -- Evaluate the left hand side, ignore the value,
                                        -- and evaluate the right hand side.

  ECase x alts -> todo -- Evaluate the variable x and select the the matching alternatives to the
                       -- to the value, similar how the pattern matching happened in the EBind,
                       -- use your intuition.
  Alt _apat body -> todo -- Ignore the pattern in the Alt and evaluate the body

  SApp fn ps -> todo -- Call a function. Lookup the function which can be external or internal
                     -- bind the values to the function parameters, or pass them to the
                     -- interpreter of the externals

  overGenerative -> error $ show overGenerative

-- The Val and Val should be separated as Literal and Value for the interpreter
literal :: Val -> Interpreter IO Val
literal _ = todo -- Turn the literal

grinMain :: Program -> Exp
grinMain = \case
  (Program _ defs) -> head $ flip mapMaybe defs $ \case
                        (Def n _ b) -> if n == "main" then Just b else Nothing
                        _           -> Nothing
  _                -> error "grinMain"

programToDefs :: Program -> Map.Map Name Exp
programToDefs = \case
  (Program _ defs) -> Map.fromList ((\d@(Def n _ _) -> (n,d)) <$> defs)
  _                -> mempty