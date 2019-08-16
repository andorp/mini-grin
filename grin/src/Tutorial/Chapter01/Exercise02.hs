{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}
module Tutorial.Chapter01.Exercise02 where

import Data.Int
import Data.Word
import Data.Maybe
import Data.Monoid (First(..))
import Grin.Exp
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.RWS.Strict (RWST(..))

import Grin.Interpreter.Env (Env)
import qualified Grin.Interpreter.Env as Env
import Grin.Interpreter.Store (Store)
import qualified Grin.Interpreter.Store as Store
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
  (a,store,()) <- runRWST r Env.empty Store.empty
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

data Node = N { nodeTag :: Grin.Tag, nodeFields :: [SValue] }
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
valueOf name = asks (`Env.lookup` name)

svalueOf :: Grin.Name -> Interpreter IO SValue
svalueOf name = do
  (Prim sv) <- valueOf name
  pure sv

alloc :: Interpreter IO Address
alloc = gets Store.size

deref :: Address -> Interpreter IO Node
deref addr = gets (Store.lookup addr)

















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
    modify (Store.insert addr node)
    pure $ Prim $ SLoc addr

  -- Fetch a value from the heap, addressed by
  -- the memory location stored in the variable h.
  SFetch h -> do
    SLoc addr <- svalueOf h
    Node <$> deref addr

  -- Update the value of the memory location h
  -- with the stored value n.
  SUpdate h n -> do
    SLoc addr <- svalueOf h
    Node node <- valueOf n
    modify (Store.insert addr node)
    pure Unit

  -- Evaluate the left hand side, ignore the value,
  -- and evaluate the right hand side.
  EBind lhs BUnit rhs -> do
    void $ eval ctx lhs
    eval ctx rhs

  -- Evaluate the left hand side, bind its value to the variable x
  -- extending the environment, then evaluate the right hand side
  -- Corresponds to the bind: lhs >>= \pattern -> rhs
  EBind lhs (BVar x) rhs -> do
    val <- eval ctx lhs
    local (Env.insert [(x, val)]) (eval ctx rhs)

  -- Evaluate the left hand side, bind its value to the pattern
  -- if the node value match the given pattern, otherwise the behaviour
  -- is undefined.
  EBind lhs (BNodePat t xs) rhs -> do
    val <- eval ctx lhs
    let Just bindings = matchNode val (t, xs)
    local (Env.insert bindings) (eval ctx rhs)

  -- Evaluate the variable x and select the the matching alternatives to the
  -- to the value, similar how the pattern matching happened in the EBind,
  -- use your intuition.
  ECase x alts -> do
    v <- valueOf x
    let mkCont (Alt pat cont) = case matchPattern v pat of
          Just bindings -> Just (local (Env.insert bindings) (eval ctx cont))
          Nothing -> Nothing
        mkCont other = error ("eval: not an Alt: " <> show other)
    let Just selectedAlt = getFirst (foldMap (First . mkCont) alts)
    selectedAlt

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
        local (const $ Env.insert (funParamNames `zip` callValues) Env.empty)
              (eval ctx body)
      overGenerative -> error $ show overGenerative

  overGenerative -> error $ show overGenerative

matchPattern :: Value -> CPat -> Maybe [(Grin.Name, Value)]
matchPattern = curry $ \case
  (Prim prim, LitPat litPat) ->
    if matchSimpleValue prim litPat
      then Just []
      else Nothing

  (val, NodePat tag fields) ->
    matchNode val (tag, fields)

  (_, DefaultPat) ->
    Just []

  (_, _) ->
    Nothing

matchNode :: Value -> (Grin.Tag, [Grin.Name]) -> Maybe [(Grin.Name, Value)]
matchNode (Node (N tag fields)) (patTag, patFields)
  | tag == patTag && length fields == length patFields
  = Just (zip patFields (map Prim fields))

matchNode _ _
  = Nothing

matchSimpleValue :: SValue -> Grin.SimpleValue -> Bool
matchSimpleValue = curry $ \case
  (SInt64 x, Grin.SInt64 y)   -> x == y
  (SWord64 x, Grin.SWord64 y) -> x == y
  (SFloat x, Grin.SFloat y)   -> x == y
  (SBool x, Grin.SBool y)     -> x == y
  (SString x, Grin.SString y) -> x == y
  (SChar x, Grin.SChar y)     -> x == y
  _ -> False


















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
