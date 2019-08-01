{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, InstanceSigs, TypeFamilies, TemplateHaskell, ScopedTypeVariables #-}
module Grin.Interpreter.Base where

import Data.Function (fix)
import qualified Data.Map as Map
import Control.Monad.Fail
import Grin.Exp hiding (Val)
import qualified Grin.Exp as Grin
import Data.Maybe (fromJust, mapMaybe, fromMaybe)
import Control.Monad.Trans (MonadIO(liftIO), lift)
import Data.List (foldl')
import Debug.Trace (traceShowId)


newtype Env v = Env (Map.Map Name v)
  deriving (Eq, Show, Ord)

emptyEnv :: Env v
emptyEnv = Env mempty

lookupEnv :: (Env v) -> Name -> v
lookupEnv (Env m) n = fromMaybe (error $ "Missing:" ++ show n) $ Map.lookup n m

extendEnv :: Env v -> [(Name, v)] -> Env v
extendEnv (Env m) vs = Env $ foldl' (\n (k,v) -> Map.insert k v n) m vs

newtype Store a v = Store (Map.Map a v)
  deriving (Eq, Ord, Show)

emptyStore :: (Ord a) => Store a v
emptyStore = Store mempty

storeFind :: (Ord a) => Store a v -> a -> v
storeFind (Store m) a = fromMaybe (error "Store; missing") $ Map.lookup a m

storeExt :: (Ord a) => a -> v -> Store a v -> Store a v
storeExt a v (Store m) = Store (Map.insert a v m)

instance (Ord a, Semigroup v) => Semigroup (Store a v) where
  (Store ma) <> (Store mb) = Store (Map.unionWith (<>) ma mb)

instance (Ord a, Monoid v) => Monoid (Store a v) where
  mempty = Store mempty

class (Monad m, MonadFail m) => Interpreter m where
  type Val     m :: *
  type HeapVal m :: *
  type StoreVal m :: *
  type Addr    m :: *

  -- Conversions, but m type is needed for type inference
  value       :: Grin.Val -> m (Val m)
  val2addr    :: Val m -> m (Addr m)
  addr2val    :: Addr m -> m (Val m)
  heapVal2val :: HeapVal m -> m (Val m)
  val2heapVal :: Val m -> m (HeapVal m)
  unit        :: m (Val m) -- The unit value
  bindPattern :: Val m -> (Tag, [Name]) -> m [(Name, Val m)]

  -- Non-pure

  -- | Return the computational environment
  askEnv      :: m (Env (Val m))
  -- | Set the local environment
  localEnv    :: Env (Val m) -> m (Val m) -> m (Val m)
  lookupFun   :: Name -> m Exp
  isOperation :: Name -> m Bool
  operation   :: Name -> [Val m] -> m (Val m)

  -- Control-flow
  evalCase    :: (Exp -> m (Val m)) -> Val m -> [Alt] -> m (Val m)
  funCall     :: (Exp -> m (Val m)) -> Name -> [Val m] -> m (Val m)

  -- Store
  getStore     :: m (Store (Addr m) (StoreVal m))
  putStore     :: (Store (Addr m) (StoreVal m)) -> m ()
  updateStore  :: (Store (Addr m) (StoreVal m) -> Store (Addr m) (StoreVal m)) -> m ()
  nextLocStore :: Store (Addr m) (StoreVal m) -> m (Addr m)
  allocStore   :: m (Val m)
  findStore    :: Val m -> m (Val m)
  extStore     :: Val m -> Val m -> m ()

grinMain :: Program -> Exp
grinMain (Program _ defs) = gmain
  where
    gmain = head $ mapMaybe (\(Def n _ b) -> if n == "main" then Just b else Nothing) defs

debug :: (Interpreter m, MonadIO m, Show v, v ~ Val m) => (Exp -> m (Val m)) -> Exp -> m (Val m)
debug ev e = do
  p <- askEnv
  liftIO $ case e of
    SApp fn ps -> print (fn, ps)
    _          -> pure ()
  ev e

-- Open recursion and monadic interpreter.
ev :: (MonadIO m, Interpreter m, a ~ Addr m, v ~ Val m, Show v) => (Exp -> m (Val m)) -> Exp -> m (Val m)
ev ev = \case
  SPure n@(ConstTagNode{})  -> value n
  SPure l@(Lit{})           -> value l
  SPure v@(Var n)           -> do
    p <- askEnv
    pure $ lookupEnv p n

  SApp fn ps -> do
    p <- askEnv
--    liftIO $ print (fn, ps, p)
    vs <- pure $ map (lookupEnv p) ps
    op <- isOperation fn
    (if op then operation else funCall ev) fn vs

  SStore n -> do
    p <- askEnv
    let v = lookupEnv p n
    a <- allocStore
    extStore a v
    pure a

  SFetch n -> do
    p <- askEnv
    let v = lookupEnv p n
    findStore v

  SUpdate nl nn -> do
    p <- askEnv
    let vl = lookupEnv p nl
    let vn = lookupEnv p nn
    extStore vl vn
    unit

  ECase n alts -> do
    p <- askEnv
    v <- pure $ lookupEnv p n
    -- Select the alternative and continue the evaluation
    evalCase ev v alts

  EBind lhs (Var n) rhs -> do
    v <- ev lhs
    p <- askEnv
    let p' = extendEnv p [(n, v)]
    localEnv p' (ev rhs)

  EBind lhs c@(ConstTagNode t@(Tag{}) vs) rhs -> do
    v   <- ev lhs
    p   <- askEnv
    p'  <- extendEnv p <$> bindPattern v (t,vs)
    localEnv p' (ev rhs)

  EBind lhs Unit rhs -> do
    _ <- ev lhs
    ev rhs

  Alt _ body -> ev body

  other -> error $ show ("ev", other)

eval :: (Interpreter m, MonadIO m, Show v, v ~ Val m) => Exp -> m v
eval e = fix (debug . ev) e

programToDefs :: Program -> Map.Map Name Exp
programToDefs (Program _ defs) = Map.fromList ((\d@(Def n _ _) -> (n,d)) <$> defs)

-- * Test expression

add =
  Program
    [ External "prim_int_add" (TySimple T_Int64) [TySimple T_Int64, TySimple T_Int64] False PrimOp
    ]
    [ Def "add" ["s1", "s2"] (SApp "prim_int_add" ["s1", "s2"])
    , Def "main" [] $
        EBind (SPure (Lit (LInt64 10))) (Var "m1") $
        EBind (SPure (Lit (LInt64 20))) (Var "m2") $
        SApp "add" ["m1", "m2"]
    ]

fact =
  Program
    [ External "prim_int_sub"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False PrimOp
    , External "prim_int_mul"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False PrimOp
    , External "prim_int_eq"    (TySimple T_Bool)   [TySimple T_Int64, TySimple T_Int64] False PrimOp
    , External "prim_int_print" (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] True  PrimOp
    ]
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

sumSimple =
  Program
    [ External "prim_int_add"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False PrimOp
    , External "prim_int_sub"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False PrimOp
    , External "prim_int_eq"    (TySimple T_Bool)   [TySimple T_Int64, TySimple T_Int64] False PrimOp
    , External "prim_int_gt"    (TySimple T_Bool)   [TySimple T_Int64, TySimple T_Int64] False PrimOp
    , External "prim_int_print" (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] True  PrimOp
    ]
    [ Def "main" [] $
        EBind (SPure (Lit (LInt64 1))) (Var "m1") $
        EBind (SPure (Lit (LInt64 100))) (Var "m2") $
        EBind (SPure (ConstTagNode (Tag C "Int") ["m1"])) (Var "m3") $
        EBind (SPure (ConstTagNode (Tag C "Int") ["m2"])) (Var "m4") $
        EBind (SStore "m3") (Var "m5") $
        EBind (SStore "m4") (Var "m6") $
        EBind (SPure (ConstTagNode (Tag F "upto") ["m5", "m6"])) (Var "m7") $
        EBind (SStore "m7") (Var "m8") $
        EBind (SPure (ConstTagNode (Tag F "sum") ["m8"])) (Var "m9") $
        EBind (SStore "m9") (Var "m10") $
        EBind (SApp "eval" ["m10"]) (ConstTagNode (Tag C "Int") ["m11"]) $
        SApp "prim_int_print" ["m11"]
    , Def "upto" ["u1", "u2"] $
        EBind (SApp "eval" ["u1"]) (ConstTagNode (Tag C "Int") ["u3"]) $
        EBind (SApp "eval" ["u2"]) (ConstTagNode (Tag C "Int") ["u4"]) $
        EBind (SApp "prim_int_gt" ["u3", "u4"]) (Var "u5") $
        ECase "u5"
          [ Alt (LitPat (LBool True)) $
                SPure (ConstTagNode (Tag C "Nil") [])
          , Alt (LitPat (LBool False)) $
                EBind (SPure (Lit (LInt64 1))) (Var "u6") $
                EBind (SApp "prim_int_add" ["u3", "u6"]) (Var "u7") $
                EBind (SPure (ConstTagNode (Tag C "Int") ["u7"])) (Var "u8") $
                EBind (SStore "u8") (Var "u9") $
                EBind (SPure (ConstTagNode (Tag F "upto") ["u9", "u2"])) (Var "u10") $
                EBind (SStore "u10") (Var "u11") $
                SPure (ConstTagNode (Tag C "Cons") ["u1", "u11"])
          ]
    , Def "sum" ["s1"] $
        EBind (SApp "eval" ["s1"]) (Var "s2") $
        ECase "s2"
          [ Alt (NodePat (Tag C "Nil") []) $
                EBind (SPure (Lit (LInt64 0))) (Var "s3") $
                SPure (ConstTagNode (Tag C "Int") ["s3"])
          , Alt (NodePat (Tag C "Cons") ["s5", "s6"]) $
                EBind (SApp "eval" ["s5"]) (ConstTagNode (Tag C "Int") ["s7"]) $
                EBind (SApp "sum" ["s6"]) (ConstTagNode (Tag C "Int") ["s8"]) $
                EBind (SApp "prim_int_add" ["s7", "s8"]) (Var "s9") $
                SPure (ConstTagNode (Tag C "Int") ["s9"])
          ]
    , Def "eval" ["e1"] $
        EBind (SFetch "e1") (Var "e2") $
        ECase "e2"
          [ Alt (NodePat (Tag C "Int") ["e3"]) $
                SPure (ConstTagNode (Tag C "Int") ["e3"])
          , Alt (NodePat (Tag C "Nil") []) $
                SPure (ConstTagNode (Tag C "Nil") [])
          , Alt (NodePat (Tag C "Cons") ["e4", "e5"]) $
                SPure (ConstTagNode (Tag C "Cons") ["e4", "e5"])
          , Alt (NodePat (Tag F "upto") ["e6", "e7"]) $
                EBind (SApp "upto" ["e6", "e7"]) (Var "e8") $
                EBind (SUpdate "e1" "e8") Unit $
                SPure (Var "e8")
          , Alt (NodePat (Tag F "sum") ["e9"]) $
                EBind (SApp "sum" ["e9"]) (Var "e10") $
                EBind (SUpdate "e1" "e10") Unit $
                SPure (Var "e10")
          ]
    ]
