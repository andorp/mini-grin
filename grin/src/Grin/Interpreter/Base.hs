{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, InstanceSigs, TypeFamilies, TemplateHaskell, ScopedTypeVariables #-}
module Grin.Interpreter.Base where

import Data.Function (fix)
import qualified Data.Map as Map
import Control.Monad.Fail
import Grin.Exp
import Data.Maybe (fromJust, mapMaybe, fromMaybe)
import Control.Monad.Trans (MonadIO(liftIO), lift)
import Data.List (foldl')
import Debug.Trace (traceShowId)


newtype Env v = Env (Map.Map Name v)
  deriving (Show)

emptyEnv :: Env v
emptyEnv = Env mempty

lookupEnv :: (Env v) -> Name -> v
lookupEnv (Env m) n = fromMaybe (error $ "Missing:" ++ show n) $ Map.lookup n m

extendEnv :: Env v -> [(Name, v)] -> Env v
extendEnv (Env m) vs = Env $ foldl' (\n (k,v) -> Map.insert k v n) m vs

newtype Store a v = Store (Map.Map a v)
  deriving (Show)

class (Ord a) => Address a where
  addrFromInt :: Int -> a
  intFromAddr :: a -> Int

storeLookup :: (Address a) => Store a v -> a -> v
storeLookup (Store m) a = fromMaybe (error "Store; missing") $ Map.lookup a m

storeSize :: (Address a) => Store a v -> a
storeSize (Store m) = addrFromInt (Map.size m)

storeExt :: (Address a) => a -> v -> Store a v -> Store a v
storeExt a v (Store m) = Store (Map.insert a v m)

class (Monad m, MonadFail m) => Interpreter m where
  type IntpVal  m :: * -- TODO: Separate Val and Node for heap
  type IAddr    m :: *
  -- pure conversions, but m type is needed for type inference
  value       :: Val  -> m (IntpVal m)
  val2addr    :: (IntpVal m) -> m (IAddr m)
  addr2val    :: (IAddr m) -> m (IntpVal m)
  unit        :: m (IntpVal m) -- The unit value
  matchNode   :: (IntpVal m) -> (Tag, [Name]) -> m [(Name, IntpVal m)]
  -- non-pure
  askEnv      :: m (Env (IntpVal m))
  localEnv    :: Env (IntpVal m) -> m (IntpVal m) -> m (IntpVal m)
  lookupFun   :: Name -> m Exp
  isOperation :: Name -> m Bool
  operation   :: Name -> [IntpVal m] -> m (IntpVal m)
  ecase       :: (Exp -> m (IntpVal m)) -> IntpVal m -> [Alt] -> m (IntpVal m)
  getStore    :: m (Store (IAddr m) (IntpVal m))
  updateStore :: (Store (IAddr m) (IntpVal m) -> Store (IAddr m) (IntpVal m)) -> m ()

findStore :: (Interpreter m, Address a, a ~ (IAddr m)) => (IntpVal m) -> m (IntpVal m)
findStore l = do
  s <- getStore
  a <- val2addr l
  pure $ storeLookup s a

extStore :: (Interpreter m, Address a, a ~ (IAddr m)) => (IntpVal m) -> (IntpVal m) -> m ()
extStore l n = do
  a <- val2addr l
  updateStore (storeExt a n)

allocStore :: (Interpreter m, Address a, a ~ IAddr m) => m (IntpVal m)
allocStore = do
  s <- getStore
  addr2val $ storeSize s

grinMain :: Program -> Exp
grinMain (Program _ defs) = gmain
  where
    gmain = head $ mapMaybe (\(Def n _ b) -> if n == "main" then Just b else Nothing) defs

debug :: (Interpreter m, MonadIO m, Show v, v ~ (IntpVal m)) => (Exp -> m (IntpVal m)) -> Exp -> m (IntpVal m)
debug ev e = do
  p <- askEnv
  liftIO $ case e of
    SApp fn ps -> print (fn, ps)
    _          -> pure ()
  ev e


ev :: (Interpreter m, a ~ IAddr m, Address a) => (Exp -> m (IntpVal m)) -> Exp -> m (IntpVal m)
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
    -- Select the
    ecase ev v alts

  EBind lhs (Var n) rhs -> do
    v <- ev lhs
    p <- askEnv
    let p' = extendEnv p [(n, v)]
    localEnv p' (ev rhs)

  EBind lhs c@(ConstTagNode t@(Tag{}) vs) rhs -> do
    v   <- ev lhs
    p   <- askEnv
    p'  <- extendEnv p <$> matchNode v (t,vs)
    localEnv p' (ev rhs)

  EBind lhs Unit rhs -> do
    _ <- ev lhs
    ev rhs

  other -> error $ show ("ev", other)

eval :: (Interpreter m, MonadIO m, a ~ IAddr m, Address a, Show v, v ~ IntpVal m) => Exp -> m v
eval e = fix ev e

-- * Test expression

add =
  Program
    []
    [ Def "add" ["s1", "s2"] (SApp "prim_int_add" ["s1", "s2"])
    , Def "main" [] $
        EBind (SPure (Lit (LInt64 10))) (Var "m1") $
        EBind (SPure (Lit (LInt64 20))) (Var "m2") $
        SApp "add" ["m1", "m2"]
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

sumSimple =
  Program
    []
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
