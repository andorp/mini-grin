{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}
module Tutorial.Chapter01.Exercise02 where

import Data.Maybe
import Grin.Exp
import Grin.Value
import Grin.Interpreter.Env
import Grin.Interpreter.Store
import Grin.TypeEnv (Ty(TySimple), SimpleType(T_Unit))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.RWS.Strict (RWST(..))
import Data.Traversable (for)
import qualified Data.Map.Strict as Map


data Context = Context
  { externalCall :: External -> [Value] -> IO Value
  , functions    :: Map.Map Name Exp
  }

newtype Heap = Heap Int
newtype Interpreter m a = Interpreter (RWST (Env Value) () (Store Int NodeValue) m a)
  deriving  ( Functor
            , Applicative
            , Monad
            , MonadReader (Env Value)
            , MonadState (Store Int NodeValue)
            )

runInterpreter
  :: (Monad m, MonadIO m)
  => Interpreter m a -> m (a, Store Int NodeValue)
runInterpreter (Interpreter r) = do
  (a,store,()) <- runRWST r emptyEnv emptyStore
  pure (a,store)

type InterpretExternal = External -> [Value] -> IO Value

-- Implement the external calls for the externals used in the Examples
externalCalls :: External -> [Value] -> IO Value
externalCalls ext args = case eName ext of
  "prim_int_eq" -> case args of
    [VLit (LInt64 a), VLit (LInt64 b)] -> pure $ VLit (LBool (a == b))
    _ -> error ("prim_int_add: invalid args: " <> show args)
  "prim_int_add" -> case args of
    [VLit (LInt64 a), VLit (LInt64 b)] -> pure $ VLit (LInt64 (a + b))
    _ -> error ("prim_int_add: invalid args: " <> show args)
  other -> error ("non-existing external: " <> show other)


interpret :: InterpretExternal -> Program -> IO Value
interpret ietx prog =
  fst <$> runInterpreter (eval (Context ietx (programToDefs prog)) (grinMain prog))

todo :: Interpreter m a
todo = error "TODO"

data NodeValue = NodeValue
  { nodeTag :: Tag
  , nodeFields :: [Value]
  }
  deriving (Eq, Show)

data Value
  = VLoc Int
  | VNode NodeValue
  | VUnit
  | VLit Lit
  | VVar Name
  deriving (Eq, Show)

nextLocation :: Monad m => Interpreter m Int
nextLocation = gets (\(Store m) -> maximum (Map.keys m))

-- NOTE: why are we passing the context manually?)

-- Write a function that interprets the given expression. The function gets the body of the main.
eval :: Context -> Exp -> Interpreter IO Value
eval ctx = \case

  SPure n@(CNode{}) ->
    -- Convert a node literal value to the grin interpreter node value
    literal n
  SPure l@(Lit{}) ->
    -- Convert a simple literal value to the grin interpreter literal value
    literal l
  SPure u@Unit ->
    -- Convert a unit literal value to the grin interpreter value
    literal u
  SPure (Var n) ->
    -- Lookup a variable in the environment and return its value
    asks (flip lookupEnv n)

  SStore v -> do
    -- Create a memory location on the heap and store the value which the variable v has.
    n@NodeValue{} <- asks (flip lookupEnv v) >>= \case
      VNode n -> pure n
      other -> error ("value should have been a Node: " <> show other)
    loc <- nextLocation
    modify (storeExt loc n)
    pure (VLoc loc)

  SFetch h -> do
    -- Fetch a value from the heap, addressed by
    -- the memory location stored in the variable h.
    loc <- asks (flip lookupEnv h) >>= \case
      VLoc loc -> pure loc
      other -> error ("value should have been a Loc: " <> show other)
    gets (VNode . flip storeFind loc)

  SUpdate h n -> do
    -- Update the value of the memory location h
    -- with the stored value n.
    loc <- asks (flip lookupEnv h) >>= \case
      VLoc loc -> pure loc
      other -> error ("value should have been a Loc: " <> show other)
    node <- asks (flip lookupEnv n) >>= \case
      VNode node -> pure node
      other -> error ("value should have been a Node: " <> show other)
    modify (storeExt loc node)
    pure (VUnit)

  EBind lhs (BVar x) rhs -> do
    -- Evaluate the left hand side, bind its value to the variable x
    -- extending the environment, then evaluate the right hand side
    val <- eval ctx lhs
    local (flip extendEnv [(x, val)]) (eval ctx rhs)
  EBind lhs (BNodePat t xs) rhs -> do
    -- Evaluate the left hand side, bind its value to the pattern
    -- if the node value match the given pattern, otherwise the behaviour
    -- is undefined.
    eval ctx lhs >>= \val ->
      case match val t xs rhs of
        Nothing -> error ("eval ECase: failed pattern match: " <> show val)
        Just cont -> cont

    where
      match :: Value -> Tag -> [Name] -> Exp -> Maybe (Interpreter IO Value)
      match val tag fields rhs = Just (error "TODO: EBind/match")

  EBind lhs BUnit rhs -> do
    -- Evaluate the left hand side, ignore the value,
    -- and evaluate the right hand side.
    _val <- eval ctx lhs
    eval ctx rhs

  ECase x alts -> do
    -- Evaluate the variable x and select the the matching alternatives to the
    -- to the value, similar how the pattern matching happened in the EBind,
    -- use your intuition.
    asks (flip lookupEnv x) >>= \val ->
      case findFirst (match val . asAlt) alts of
        Nothing ->
          error ("eval ECase: failed pattern match: " <> show val)
        Just (bindings, cont) ->
          local (flip extendEnv bindings) (eval ctx cont)

    where
      findFirst :: (a -> Maybe b) -> [a] -> Maybe b
      findFirst f xs = case mapMaybe f xs of
        [] -> Nothing
        (y:_) -> Just y

      asAlt (Alt pat rhs) = (pat, rhs)
      asAlt other = error ("eval ECase: case alternative is not an Alt: " <> show other)

      -- TODO: cont is not really used here
      match :: Value -> (CPat, Exp) -> Maybe ([(Name, Value)], Exp)
      match val (pat, cont) = case (val, pat) of

        (_, DefaultPat) ->
          Just ([], cont)

        (VLit lit, LitPat litPat) ->
          if lit == litPat
            then Just ([], cont)
            else Nothing

        (VNode (NodeValue tag fields), NodePat patTag patFields) ->
          if tag == patTag && length fields == length patFields
            then Just (error "there's a problem")
            else Nothing

        _other ->
          Nothing

  Alt _apat body -> do
    -- Ignore the pattern in the Alt and evaluate the body
    eval ctx body

  SApp fn ps -> do
    -- Call a function. Lookup the function which can be external or internal
    -- bind the values to the function parameters, or pass them to the
    -- interpreter of the externals
    case Map.lookup fn (functions ctx) of
      Just (Def functionName argNames body) -> do
        params <- for (zip argNames ps) $ \(argName, p) -> do
          val <- asks (flip lookupEnv p)
          pure (argName, val)
        -- new environment!
        local (const (extendEnv emptyEnv params)) (eval ctx body)
      Just other ->
        error ("eval SApp: function is not a Def: " <> show other)
      Nothing -> do
        -- we just make it up here
        let external = External fn (TySimple T_Unit) [] True PrimOp
        vals <- for ps $ \name -> do
          asks (flip lookupEnv name)
        Interpreter (lift (externalCall ctx external vals))

  overGenerative ->
    error $ show overGenerative

-- The Val and Val should be separated as Literal and Value for the interpreter
literal :: Val -> Interpreter IO Value
literal _ = todo

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
