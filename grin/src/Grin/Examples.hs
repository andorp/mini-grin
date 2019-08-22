{-# LANGUAGE DataKinds #-}
module Grin.Examples where

import Grin.Value
  ( VarOrValue(Var, Val)
  , Tag(Tag)
  , TagType(C,F)
  , Node(Node)
  , Value(VNode, VPrim)
  , SimpleValue(SInt64,SBool)
  )

import Grin.TypeEnv
  ( SimpleType(T_Int64, T_Bool, T_Unit)
  , Ty(TySimple)
  )

import Grin.Exp
  ( External(External)
  , BPat(BUnit,BVar,BNodePat)
  , CPat(NodePat, LitPat)
  )

import Grin.GExp
  ( Exp(Program,Def,Case,Alt,App,Store,Fetch,Update,Bind,Pure)
  , ExpCtx(Prg)
  )

-- * Test expression

{-
primop pure
  prim_int_add :: T_Int64 -> T_Int64 -> T_Int64

add s1 s2 =
  s3 <- prim_int_add $ s1 s2
  pure s3

main =
  m1 <- pure 10
  m2 <- pure 20
  add $ m1 m2
-}

add :: Exp 'Prg
add =
  Program
    -- type signatures of external functions must be provided at the top of teh module
    [ External "prim_int_add" (TySimple T_Int64) [TySimple T_Int64, TySimple T_Int64] False
    ]
    [ Def "add" ["s1", "s2"] $
        Bind (App "prim_int_add" ["s1", "s2"]) (BVar "s3") $
        Pure (Var "s3")
    , Def "main" [] $
        Bind (Pure (Val (VPrim (SInt64 10)))) (BVar "m1") $
        Bind (Pure (Val (VPrim (SInt64 20)))) (BVar "m2") $
        App "add" ["m1", "m2"]
    ]

-- * Factorial

{-
prim_int_sub :: T_Int64 -> T_Int64 -> T_Int64
prim_int_mul :: T_Int64 -> T_Int64 -> T_Int64
prim_int_eq :: T_Int64 -> T_Int64 -> T_Bool
prim_int_print :: T_Int64 -> T_Int64 -> T_Unit

fact f1 =
  f2 <- pure 0
  f3 <- prim_int_eq $ f1 f2
  case f3 of
    #True ->
      f7 <- pure 1
      pure f7
    #False ->
      f4 <- pure 1
      f5 <- prim_int_sub $ f1 f4
      f6 <- fact $ f5
      prim_int_mul $ f1 f6

main =
  m1 <- pure 10
  m2 <- fact $ m1
  prim_int_print $ m2
  pure m2
-}

fact :: Exp 'Prg
fact =
  Program
    [ External "prim_int_sub"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False
    , External "prim_int_mul"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False
    , External "prim_int_eq"    (TySimple T_Bool)   [TySimple T_Int64, TySimple T_Int64] False
    , External "prim_int_print" (TySimple T_Unit)   [TySimple T_Int64, TySimple T_Int64] True
    ]
    [ Def "fact" ["f1"] $
        Bind (Pure (Val (VPrim (SInt64 0)))) (BVar "f2") $
        Bind (App "prim_int_eq" ["f1", "f2"]) (BVar "f3") $
        Case "f3"
          [ Alt (LitPat (SBool True)) $
                Bind (Pure (Val (VPrim (SInt64 1)))) (BVar "f7") $
                Pure (Var "f7")
          , Alt (LitPat (SBool False)) $
                Bind (Pure (Val (VPrim (SInt64 1)))) (BVar "f4") $
                Bind (App "prim_int_sub" ["f1", "f4"]) (BVar "f5") $
                Bind (App "fact" ["f5"]) (BVar "f6") $
                App "prim_int_mul" ["f1", "f6"]
          ]
    , Def "main" [] $
        Bind (Pure (Val (VPrim (SInt64 10)))) (BVar "m1") $
        Bind (App "fact" ["m1"]) (BVar "m2") $
        Bind (App "prim_int_print" ["m2"]) BUnit $
        Pure (Var "m2")
    ]

-- * Sum simple

{-
prim_int_add :: T_Int64 -> T_Int64 -> T_Int64
prim_int_sub :: T_Int64 -> T_Int64 -> T_Int64
prim_int_eq :: T_Int64 -> T_Int64 -> T_Bool
prim_int_gt :: T_Int64 -> T_Int64 -> T_Bool
prim_int_print :: T_Int64 -> T_Int64 -> T_Unit

main =
  m1 <- pure 1
  m2 <- pure 100
  m3 <- pure (CInt m1)
  m4 <- pure (CInt m2)
  m5 <- store m3
  m6 <- store m4
  m7 <- pure (Fupto m5 m6)
  m8 <- store m7
  m9 <- pure (Fsum m8)
  m10 <- store m9
  (CInt m11) <- eval $ m10
  prim_int_print $ m11

upto u1 u2 =
  (CInt u3) <- eval $ u1
  (CInt u4) <- eval $ u2
  u5 <- prim_int_gt $ u3 u4
  case u5 of
    #True ->
      u12 <- pure (CNil)
      pure u12
    #False ->
      u6 <- pure 1
      u7 <- prim_int_add $ u3 u6
      u8 <- pure (CInt u7)
      u9 <- store u8
      u10 <- pure (Fupto u9 u2)
      u11 <- store u10
      pure (CCons u1 u11)

sum s1 =
  s2 <- eval $ s1
  case s2 of
    (CNil) ->
      s3 <- pure 0
      pure (CInt s3)
    (CCons s5 s6) ->
      (CInt s7) <- eval $ s5
      (CInt s8) <- sum $ s6
      s9 <- prim_int_add $ s7 s8
      pure (CInt s9)

eval e1 =
  e2 <- fetch e1
  case e2 of
    (CInt e3) ->
      e11 <- pure (CInt e3)
      pure e11
    (CNil) ->
      e12 <- pure (CNil)
      pure e12
    (CCons e4 e5) ->
      e13 <- pure (CCons e4 e5)
      pure e13
    (Fupto e6 e7) ->
      e8 <- upto $ e6 e7
      update e1 e8
      pure e8
    (Fsum e9) ->
      e10 <- sum $ e9
      update e1 e10
      pure e10
-}

sumSimple :: Exp 'Prg
sumSimple =
  Program
    [ External "prim_int_add"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False
    , External "prim_int_sub"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False
    , External "prim_int_eq"    (TySimple T_Bool)   [TySimple T_Int64, TySimple T_Int64] False
    , External "prim_int_gt"    (TySimple T_Bool)   [TySimple T_Int64, TySimple T_Int64] False
    , External "prim_int_print" (TySimple T_Unit)   [TySimple T_Int64, TySimple T_Int64] True
    ]
    [ Def "main" [] $
        Bind (Pure (Val (VPrim (SInt64 1)))) (BVar "m1") $
        Bind (Pure (Val (VPrim (SInt64 100)))) (BVar "m2") $
        Bind (Pure (Val (VNode (Node (Tag C "Int") ["m1"])))) (BVar "m3") $
        Bind (Pure (Val (VNode (Node (Tag C "Int") ["m2"])))) (BVar "m4") $
        Bind (Store "m3") (BVar "m5") $
        Bind (Store "m4") (BVar "m6") $
        Bind (Pure (Val (VNode (Node (Tag F "upto") ["m5", "m6"])))) (BVar "m7") $
        Bind (Store "m7") (BVar "m8") $
        Bind (Pure (Val (VNode (Node (Tag F "sum") ["m8"])))) (BVar "m9") $
        Bind (Store "m9") (BVar "m10") $
        Bind (App "eval" ["m10"]) (BNodePat (Tag C "Int") ["m11"]) $
        App "prim_int_print" ["m11"]
    , Def "upto" ["u1", "u2"] $
        Bind (App "eval" ["u1"]) (BNodePat (Tag C "Int") ["u3"]) $
        Bind (App "eval" ["u2"]) (BNodePat (Tag C "Int") ["u4"]) $
        Bind (App "prim_int_gt" ["u3", "u4"]) (BVar "u5") $
        Case "u5"
          [ Alt (LitPat (SBool True)) $
                Bind (Pure (Val (VNode (Node (Tag C "Nil") [])))) (BVar "u12") $
                Pure (Var "u12")
          , Alt (LitPat (SBool False)) $
                Bind (Pure (Val (VPrim (SInt64 1)))) (BVar "u6") $
                Bind (App "prim_int_add" ["u3", "u6"]) (BVar "u7") $
                Bind (Pure (Val (VNode (Node (Tag C "Int") ["u7"])))) (BVar "u8") $
                Bind (Store "u8") (BVar "u9") $
                Bind (Pure (Val (VNode (Node (Tag F "upto") ["u9", "u2"])))) (BVar "u10") $
                Bind (Store "u10") (BVar "u11") $
                Pure (Val (VNode (Node (Tag C "Cons") ["u1", "u11"])))
          ]
    , Def "sum" ["s1"] $
        Bind (App "eval" ["s1"]) (BVar "s2") $
        Case "s2"
          [ Alt (NodePat (Tag C "Nil") []) $
                Bind (Pure (Val (VPrim (SInt64 0)))) (BVar "s3") $
                Pure (Val (VNode (Node (Tag C "Int") ["s3"])))
          , Alt (NodePat (Tag C "Cons") ["s5", "s6"]) $
                Bind (App "eval" ["s5"]) (BNodePat (Tag C "Int") ["s7"]) $
                Bind (App "sum" ["s6"]) (BNodePat (Tag C "Int") ["s8"]) $
                Bind (App "prim_int_add" ["s7", "s8"]) (BVar "s9") $
                Pure (Val (VNode (Node (Tag C "Int") ["s9"])))
          ]
    , Def "eval" ["e1"] $
        Bind (Fetch "e1") (BVar "e2") $
        Case "e2"
          [ Alt (NodePat (Tag C "Int") ["e3"]) $
                Bind (Pure (Val (VNode (Node (Tag C "Int") ["e3"])))) (BVar "e11") $
                Pure (Var "e11")
          , Alt (NodePat (Tag C "Nil") []) $
                Bind (Pure (Val (VNode (Node (Tag C "Nil") [])))) (BVar "e12") $
                Pure (Var "e12")
          , Alt (NodePat (Tag C "Cons") ["e4", "e5"]) $
                Bind (Pure (Val (VNode (Node (Tag C "Cons") ["e4", "e5"])))) (BVar "e13") $
                Pure (Var "e13")
          , Alt (NodePat (Tag F "upto") ["e6", "e7"]) $
                Bind (App "upto" ["e6", "e7"]) (BVar "e8") $
                Bind (Update "e1" "e8") BUnit $
                Pure (Var "e8")
          , Alt (NodePat (Tag F "sum") ["e9"]) $
                Bind (App "sum" ["e9"]) (BVar "e10") $
                Bind (Update "e1" "e10") BUnit $
                Pure (Var "e10")
          ]
    ]
