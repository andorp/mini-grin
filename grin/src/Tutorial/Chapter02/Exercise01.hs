{-# LANGUAGE LambdaCase, TypeFamilies #-}
module Tutorial.Chapter02.Exercise01 where

import Control.Monad (void)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Trans (MonadIO)
import Grin.Exp (Exp(..), Alt, BPat(..))
import Grin.Value hiding (Val)

import Grin.Interpreter.Env (Env)
import qualified Grin.Interpreter.Env as Env
import qualified Grin.Value as Grin

import Tutorial.Chapter01.Exercise02 (Definitional(..))
import qualified Tutorial.Chapter01.Exercise02 as Definitional


{-
Motivation:
The talk is based on the Abstracting Definitional Interpreters [1].

In this paper they claim that the same structure for the definitional
interpreter can be reused to create abstract interpreters which
are some form of inference. We will see that type inference
can be encoded using abstract interpretation.

The main motivation here is to understand how the abstraction of
the definitional interpreter can be achieved.

The paper uses open recursion technique to make possible to inject
different aspects of the recursive calls.

This approach is also used in the `semantic` framework by github [2].

[1] https://plum-umd.github.io/abstracting-definitional-interpreters/
[2] https://github.com/github/semantic

Exercise:
Read the "2 From Machines to Compositional Evaluators"
https://plum-umd.github.io/abstracting-definitional-interpreters/#%28part._s~3aaam%29

Exercise:
Find the difference between the interpreter from the previous exercise.
-}

-- | The interpreter is written in an open recursive style;
-- the evaluator does not call itself recursively, instead it takes as an argument
-- a function ev (the argument) is called instead of self-recursion.
-- This is a standard encoding for recursive functions in a setting without recursive binding.
-- It is up to an external function, such as the Y-combinator, to close the recursive loop.
-- This open recursive form is crucial because it allows intercepting recursive calls
-- to perform “deep” instrumentation of the interpreter.

eval  :: (MonadIO m, Interpreter m)
    => (Exp -> m (Val m)) -> Exp -> m (Val m)
eval ev = \case
  SPure (Grin.Val l) -> value l
  SPure (Var n) -> do
    p <- askEnv
    pure $ Env.lookup p n

  SApp fn ps -> do
    p <- askEnv
    vs <- pure $ map (Env.lookup p) ps
    op <- isExternal fn
    (if op then external else funCall ev) fn vs

  SFetch n -> do
    p <- askEnv
    let v = Env.lookup p n
    fetchStore v

  SUpdate nl nn -> do
    p <- askEnv
    let vl = Env.lookup p nl
    let vn = Env.lookup p nn
    extStore vl vn
    unit

  ECase n alts -> do
    p <- askEnv
    v <- pure $ Env.lookup p n
    -- Select the alternative and continue the evaluation
    evalCase ev v alts

  EBind (SStore n) (BVar l) rhs -> do
    p <- askEnv
    let v = Env.lookup p n
    a  <- allocStore l
    extStore a v
    let p' = Env.insert [(l, a)] p
    localEnv p' (ev rhs)

  EBind lhs (BVar n) rhs -> do
    v <- ev lhs
    p <- askEnv
    let p' = Env.insert [(n, v)] p
    localEnv p' (ev rhs)

  EBind lhs (BNodePat t@(Tag{}) vs) rhs -> do
    v   <- ev lhs
    p   <- askEnv
    p'  <- flip Env.insert p <$> bindPattern v (t,vs)
    localEnv p' (ev rhs)

  EBind lhs BUnit rhs -> do
    void $ ev lhs
    ev rhs

  Alt _pat body -> do
    ev body

  overGenerative -> error $ show overGenerative


class (Monad m, MonadFail m) => Interpreter m where
  type Val          m :: * -- ^ Values that can be placed in registers/variables
  type HeapVal      m :: * -- ^ Values for the Store, Fetch, Update parameters
  type Addr         m :: * -- ^ A type to represent Addresses

  -- Conversions, but m type is needed for type inference
  value       :: Grin.Value   -> m (Val m)  -- Value of the given literal
  val2addr    :: Val m        -> m (Addr m) --
  addr2val    :: Addr m       -> m (Val m)
  heapVal2val :: HeapVal m    -> m (Val m)
  val2heapVal :: Val m        -> m (HeapVal m)
  unit        :: m (Val m) -- The unit value
  bindPattern :: Val m -> (Tag, [Name]) -> m [(Name, Val m)]

  -- Non-pure

  -- | Return the computational environment
  askEnv        :: m (Env (Val m))
  -- | Set the local environment
  localEnv      :: Env (Val m) -> m (Val m) -> m (Val m)
  lookupFun     :: Name -> m Exp
  isExternal    :: Name -> m Bool
  external      :: Name -> [Val m] -> m (Val m)

  -- Control-flow
  evalCase      :: (Exp -> m (Val m)) -> Val m -> [Alt] -> m (Val m)
  funCall       :: (Exp -> m (Val m)) -> Name -> [Val m] -> m (Val m)

  -- Store
  allocStore    :: Name -> m (Val m)
  fetchStore    :: Val m -> m (Val m)      -- TODO: Change this to Addr m??
  extStore      :: Val m -> Val m -> m ()  --

instance (MonadIO m, Monad m, MonadFail m) => Interpreter (Definitional m) where
  type Val     (Definitional m) = Definitional.Value
  type HeapVal (Definitional m) = Definitional.Node
  type Addr    (Definitional m) = Definitional.Address

  value       = Definitional.value
  val2addr    = Definitional.val2addr
  addr2val    = Definitional.addr2val
  heapVal2val = Definitional.heapVal2val
  val2heapVal = Definitional.val2heapVal
  unit        = Definitional.unit
  bindPattern = Definitional.bindPattern

  askEnv      = Definitional.askEnv
  localEnv    = Definitional.localEnv
  lookupFun   = Definitional.lookupFun
  isExternal  = Definitional.isExternal
  external    = Definitional.external

  evalCase    = Definitional.evalCase
  funCall     = Definitional.funCall

  allocStore  = Definitional.allocStore
  fetchStore  = Definitional.fetchStore
  extStore    = Definitional.extStore
