{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, InstanceSigs, TypeFamilies, TemplateHaskell, ScopedTypeVariables #-}
module Grin.Interpreter.Base where

import Data.Function (fix)
import qualified Data.Map as Map
import Control.Monad.Fail
import Grin.Exp
import Data.Maybe (fromJust, mapMaybe, fromMaybe)
import Control.Monad.Trans (MonadIO(liftIO), lift)
import Data.List (foldl')


newtype Env v = Env (Map.Map Name v)
  deriving (Show)

emptyEnv :: Env v
emptyEnv = Env mempty

lookupEnv :: (Env v) -> Name -> v
lookupEnv (Env m) n = fromMaybe (error $ "Missing:" ++ show n) $ Map.lookup n m

extendEnv :: Env v -> [(Name, v)] -> Env v
extendEnv (Env m) vs = Env $ foldl' (\n (k,v) -> Map.insert k v n) m vs

grinMain :: Program -> Exp
grinMain (Program _ defs) = gmain
  where
    gmain = head $ mapMaybe (\(Def n _ b) -> if n == "main" then Just b else Nothing) defs

class (Monad m, MonadFail m) => Interpreter m where
  type IntpVal m :: *
  value       :: Val  -> m (IntpVal m) -- pure conversion, but the m type parameter needs to be present
  askEnv      :: m (Env (IntpVal m))
  localEnv    :: Env (IntpVal m) -> m (IntpVal m) -> m (IntpVal m)
  lookupFun   :: Name -> m Exp
  isOperation :: Name -> m Bool
  operation   :: Name -> [IntpVal m] -> m (IntpVal m)
  ecase       :: (Exp -> m (IntpVal m)) -> IntpVal m -> [Alt] -> m (IntpVal m)

{-
debug :: (Interpreter m, MonadIO m, Show (IntpVal m)) => (Exp -> m (IntpVal m)) -> Exp -> m (IntpVal m)
debug ev e = do
  p <- askEnv
  liftIO $ print p
  ev (traceShowId e)
-}

ev :: (Interpreter m) => (Exp -> m (IntpVal m)) -> Exp -> m (IntpVal m)
ev ev = \case
  SPure n@(ConstTagNode{})  -> value n
  SPure l@(Lit{})           -> value l
  SPure v@(Var n)           -> do
    p <- askEnv
    pure $ lookupEnv p n
  SApp fn ps -> do
    p <- askEnv
    vs <- pure $ map (lookupEnv p) ps
    op <- isOperation fn
    if op
      then operation fn vs
      else do
        -- Lookup the function
        (Def _ fps body) <- lookupFun fn
        -- Extend the environment with the function parameters
        let p' = extendEnv p (fps `zip` vs)
        -- Call the eval function on the body of the function with the extended env
        localEnv p' (ev body)

  ECase n alts -> do
    p <- askEnv
    v <- pure $ lookupEnv p n
    -- Select the
    ecase ev v alts

  EBind lhs (Var n) rhs -> do
    v <- ev lhs
    p <- askEnv
    let p' = extendEnv p [(n, v)]
    localEnv p' (ev rhs)

eval :: (Interpreter m, MonadIO m) => Exp -> m (IntpVal m)
eval e = fix ev e

-- * Test expression

sump =
  Program
    []
    [ Def "sum" ["s1", "s2"] (SApp "prim_int_add" ["s1", "s2"])
    , Def "main" [] $
        EBind (SPure (Lit (LInt64 10))) (Var "m1") $
        EBind (SPure (Lit (LInt64 20))) (Var "m2") $
        SApp "sum" ["m1", "m2"]
    ]

fact =
  Program
    []
    [ Def "fact" ["f1"] $
        EBind (SPure (Lit (LInt64 0))) (Var "f2") $
        EBind (SApp "prim_int_eq" ["f1", "f2"]) (Var "f3") $
        ECase "f3"
          [ Alt (LitPat (LBool True)) $
                SPure (Lit (LInt64 1))
          , Alt (LitPat (LBool False)) $
                EBind (SPure (Lit (LInt64 1))) (Var "f4") $
                EBind (SApp "prim_int_sub" ["f1", "f4"]) (Var "f5") $
                EBind (SApp "fact" ["f5"]) (Var "f6") $
                SApp "prim_int_mul" ["f1", "f6"]
          ]
    , Def "main" [] $
        EBind (SPure (Lit (LInt64 10))) (Var "m1") $
        EBind (SApp "fact" ["m1"]) (Var "m2") $
        SApp "prim_int_print" ["m2"]
    ]
