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

import Grin.TypeEnv.Result
  ( SimpleType(T_Int64, T_Bool, T_Unit)
  , Ty(TySimple)
  )

import Grin.Exp
  ( External(External)
  , BPat(BVar,BNodePat)
  , CPat(NodePat, LitPat, DefaultPat)
  , printGrin
  )

import Grin.GExp
  ( Exp(Program,Def,Case,Alt,App,Store,Fetch,Update,Bind,Pure)
  , ExpCtx(Prg)
  )

import Grin.GExpToExp


-- * Test expression

printExamples :: IO ()
printExamples = do
--  printGrin $ gexpToExp add
--  printGrin $ gexpToExp fact
  printGrin $ gexpToExp sumSimple

{-
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
    alt1@#True ->
      f7 <- pure 1
      pure f7
    alt2@#False ->
      f4 <- pure 1
      f5 <- prim_int_sub $ f1 f4
      f6 <- fact $ f5
      prim_int_mul $ f1 f6

main =
  m1 <- pure 10
  m2 <- fact $ m1
  pip1 <- prim_int_print $ m2
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
          [ Alt "alt1" (LitPat (SBool True)) $
                Bind (Pure (Val (VPrim (SInt64 1)))) (BVar "f7") $
                Pure (Var "f7")
          , Alt "alt2" (LitPat (SBool False)) $
                Bind (Pure (Val (VPrim (SInt64 1)))) (BVar "f4") $
                Bind (App "prim_int_sub" ["f1", "f4"]) (BVar "f5") $
                Bind (App "fact" ["f5"]) (BVar "f6") $
                App "prim_int_mul" ["f1", "f6"]
          ]
    , Def "main" [] $
        Bind (Pure (Val (VPrim (SInt64 10)))) (BVar "m1") $
        Bind (App "fact" ["m1"]) (BVar "m2") $
        Bind (App "prim_int_print" ["m2"]) (BVar "pip1") $
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
  m13 <- eval $ m10
  p1@(CInt m11) <- pure m13
  m12 <- prim_int_print $ m11
  pure m12

upto u1 u2 =
  u14 <- eval $ u1
  p2@(CInt u3) <- pure u14
  u15 <- eval $ u2
  p3@(CInt u4) <- pure u15
  u5 <- prim_int_gt $ u3 u4
  u16 <- case u5 of
    alt1@#True ->
      u13 <- pure (CNil)
      pure u13
    alt2@#False ->
      u6 <- pure 1
      u7 <- prim_int_add $ u3 u6
      u8 <- pure (CInt u7)
      u9 <- store u8
      u10 <- pure (Fupto u9 u2)
      u11 <- store u10
      u12 <- pure (CCons u1 u11)
      pure u12
  pure u16

sum s1 =
  s2 <- eval $ s1
  s10 <- case s2 of
    alt3@(CNil) ->
      s3 <- pure 0
      s11 <- pure (CInt s3)
      pure s11
    alt4@(CCons s5 s6) ->
      s13 <- eval $ s5
      p4@(CInt s7) <- pure s13
      s14 <- sum $ s6
      p5@(CInt s8) <- pure s14
      s9 <- prim_int_add $ s7 s8
      s12 <- pure (CInt s9)
      pure s12
  pure s10

eval e1 =
  e2 <- fetch e1
  e14 <- case e2 of
    alt5@(CInt e3) ->
      e11 <- pure (CInt e3)
      pure e11
    alt6@(CNil) ->
      e12 <- pure (CNil)
      pure e12
    alt7@(CCons e4 e5) ->
      e13 <- pure (CCons e4 e5)
      pure e13
    alt8@(Fupto e6 e7) ->
      e8 <- upto $ e6 e7
      up1 <- update e1 e8
      pure e8
    alt9@(Fsum e9) ->
      e10 <- sum $ e9
      up2 <- update e1 e10
      pure e10
  pure e14
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
        -- Bind (App "eval" ["m10"]) (BNodePat "p1" (Tag C "Int") ["m11"]) $
        Bind (App "eval" ["m10"]) (BVar "m13") $
        Bind (Pure (Var "m13")) (BNodePat "p1" (Tag C "Int") ["m11"]) $
        Bind (App "prim_int_print" ["m11"]) (BVar "m12") $
        Pure (Var "m12")
    , Def "upto" ["u1", "u2"] $
        -- Bind (App "eval" ["u1"]) (BNodePat "p2" (Tag C "Int") ["u3"]) $
        Bind (App "eval" ["u1"]) (BVar "u14") $
        Bind (Pure (Var "u14")) (BNodePat "p2" (Tag C "Int") ["u3"]) $
        -- Bind (App "eval" ["u2"]) (BNodePat "p3" (Tag C "Int") ["u4"]) $
        Bind (App "eval" ["u2"]) (BVar "u15") $
        Bind (Pure (Var "u15")) (BNodePat "p3" (Tag C "Int") ["u4"]) $
        Bind (App "prim_int_gt" ["u3", "u4"]) (BVar "u5") $
        Bind (Case "u5"
          [ Alt "alt1" (LitPat (SBool True)) $
                Bind (Pure (Val (VNode (Node (Tag C "Nil") [])))) (BVar "u13") $
                Pure (Var "u13")
          , Alt "alt2" (LitPat (SBool False)) $
                Bind (Pure (Val (VPrim (SInt64 1)))) (BVar "u6") $
                Bind (App "prim_int_add" ["u3", "u6"]) (BVar "u7") $
                Bind (Pure (Val (VNode (Node (Tag C "Int") ["u7"])))) (BVar "u8") $
                Bind (Store "u8") (BVar "u9") $
                Bind (Pure (Val (VNode (Node (Tag F "upto") ["u9", "u2"])))) (BVar "u10") $
                Bind (Store "u10") (BVar "u11") $
                Bind (Pure (Val (VNode (Node (Tag C "Cons") ["u1", "u11"])))) (BVar "u12") $
                Pure (Var "u12")
          ]) (BVar "u16") $
        Pure (Var "u16")
    , Def "sum" ["s1"] $
        Bind (App "eval" ["s1"]) (BVar "s2") $
        Bind (Case "s2"
          [ Alt "alt3" (NodePat (Tag C "Nil") []) $
                Bind (Pure (Val (VPrim (SInt64 0)))) (BVar "s3") $
                Bind (Pure (Val (VNode (Node (Tag C "Int") ["s3"])))) (BVar "s11") $
                Pure (Var "s11")
          , Alt "alt4" (NodePat (Tag C "Cons") ["s5", "s6"]) $
                -- Bind (App "eval" ["s5"]) (BNodePat "p4" (Tag C "Int") ["s7"]) $
                Bind (App "eval" ["s5"]) (BVar "s13") $
                Bind (Pure (Var "s13")) (BNodePat "p4" (Tag C "Int") ["s7"]) $
                -- Bind (App "sum" ["s6"]) (BNodePat "p5" (Tag C "Int") ["s8"]) $
                Bind (App "sum" ["s6"]) (BVar "s14") $
                Bind (Pure (Var "s14")) (BNodePat "p5" (Tag C "Int") ["s8"]) $
                Bind (App "prim_int_add" ["s7", "s8"]) (BVar "s9") $
                Bind (Pure (Val (VNode (Node (Tag C "Int") ["s9"])))) (BVar "s12") $
                Pure (Var "s12")
          ]) (BVar "s10") $
        Pure (Var "s10")
    , Def "eval" ["e1"] $
        Bind (Fetch "e1") (BVar "e2") $
        Bind (Case "e2"
          [ Alt "alt5" (NodePat (Tag C "Int") ["e3"]) $
                Bind (Pure (Val (VNode (Node (Tag C "Int") ["e3"])))) (BVar "e11") $
                Pure (Var "e11")
          , Alt "alt6" (NodePat (Tag C "Nil") []) $
                Bind (Pure (Val (VNode (Node (Tag C "Nil") [])))) (BVar "e12") $
                Pure (Var "e12")
          , Alt "alt7" (NodePat (Tag C "Cons") ["e4", "e5"]) $
                Bind (Pure (Val (VNode (Node (Tag C "Cons") ["e4", "e5"])))) (BVar "e13") $
                Pure (Var "e13")
          , Alt "alt8" (NodePat (Tag F "upto") ["e6", "e7"]) $
                Bind (App "upto" ["e6", "e7"]) (BVar "e8") $
                Bind (Update "e1" "e8") (BVar "up1") $
                Pure (Var "e8")
          , Alt "alt9" (NodePat (Tag F "sum") ["e9"]) $
                Bind (App "sum" ["e9"]) (BVar "e10") $
                Bind (Update "e1" "e10") (BVar "up2") $
                Pure (Var "e10")
          ]) (BVar "e14") $
        Pure (Var "e14")
    ]

{-
prim_int_add :: T_Int64 -> T_Int64 -> T_Int64
prim_int_print :: T_Int64 -> T_Int64 -> T_Unit

add s1 s2 =
  (CInt k1) @ _1 <- pure s1
  (CInt k2) @ _2 <- pure s2
  k3 <- prim_int_add $ k1 k2
  pure k3

main =
  m1 <- pure 10
  m2 <- pure 20
  n1.0 <- pure (CInt m1)
  n1.1 <- pure n1.0
  n1.2 <- pure n1.1
  n2 <- pure (CInt m2)
  m3 <- add $ n1.2 n2
  pure m3
-}

foo :: Exp 'Prg
foo =
  Program
    -- type signatures of external functions must be provided at the top of teh module
    [ External "prim_int_add" (TySimple T_Int64) [TySimple T_Int64, TySimple T_Int64] False
    , External "prim_int_print" (TySimple T_Unit)   [TySimple T_Int64, TySimple T_Int64] True
    ]
    [ Def "add" ["s1", "s2"] $
        Bind (Pure (Var "s1")) (BNodePat "_1" (Tag C "Int") ["k1"]) $
        Bind (Pure (Var "s2")) (BNodePat "_2" (Tag C "Int") ["k2"]) $
        Bind (App "prim_int_add" ["k1", "k2"]) (BVar "k3") $
        Pure (Var "k3")
    , Def "main" [] $
        Bind (Pure (Val (VPrim (SInt64 10)))) (BVar "m1") $
        Bind (Pure (Val (VPrim (SInt64 20)))) (BVar "m2") $
        Bind (Pure (Val (VNode (Node (Tag C "Int") ["m1"])))) (BVar "n1.0") $
        Bind (Pure (Var "n1.0")) (BVar "n1.1") $
        Bind (Pure (Var "n1.1")) (BVar "n1.2") $
        Bind (Pure (Val (VNode (Node (Tag C "Int") ["m2"])))) (BVar "n2") $
        Bind (App "add" ["n1.2", "n2"]) (BVar "m3") $
        Pure (Var "m3")
    ]

{-
prim_int_add :: T_Int64 -> T_Int64 -> T_Int64
prim_int_print :: T_Int64 -> T_Int64 -> T_Unit

main =
  k1 <- pure 10
  n1 <- pure (CInt k1)
  n2 <- case n1 of
    (CInt k2) @ alt1 ->
      n3 <- pure (COne)
      pure n3
    (CFoo) @ alt2 ->
      n4 <- pure (CTwo)
      pure n4
    #default @ alt3 ->
      n5 <- pure (CThree)
      pure n5
  k3 <- case k1 of
    10 @ alt4 ->
      k4 <- pure 1
      pure k4
    20 @ alt5 ->
      k5 <- pure 2
      pure k5
    #default @ alt6 ->
      k6 <- pure 3
      pure k6
  pure n2
-}

foo2 :: Exp 'Prg
foo2 =
  Program
    -- type signatures of external functions must be provided at the top of teh module
    [ External "prim_int_add" (TySimple T_Int64) [TySimple T_Int64, TySimple T_Int64] False
    , External "prim_int_print" (TySimple T_Unit)   [TySimple T_Int64, TySimple T_Int64] True
    ]
    [ Def "main" [] $
        Bind (Pure (Val (VPrim (SInt64 10)))) (BVar "k1") $
        Bind (Pure (Val (VNode (Node (Tag C "Int") ["k1"])))) (BVar "n1") $
        Bind (Case "n1"
          [ Alt "alt1" (NodePat (Tag C "Int") ["k2"]) $
              Bind (Pure (Val (VNode (Node (Tag C "One") [])))) (BVar "n3") $
              Pure (Var "n3")
          , Alt "alt2" (NodePat (Tag C "Foo") []) $
              Bind (Pure (Val (VNode (Node (Tag C "Two") [])))) (BVar "n4") $
              Pure (Var "n4")
          , Alt "alt3" DefaultPat $
              Bind (Pure (Val (VNode (Node (Tag C "Three") [])))) (BVar "n5") $
              Pure (Var "n5")
          ]) (BVar "n2") $
        Bind (Case "k1"
          [ Alt "alt4" (LitPat (SInt64 10)) $
              Bind (Pure (Val (VPrim (SInt64 1)))) (BVar "k4") $
              Pure (Var "k4")
          , Alt "alt5" (LitPat (SInt64 20)) $
              Bind (Pure (Val (VPrim (SInt64 2)))) (BVar "k5") $
              Pure (Var "k5")
          , Alt "alt6" DefaultPat $
              Bind (Pure (Val (VPrim (SInt64 3)))) (BVar "k6") $
              Pure (Var "k6")
          ]) (BVar "k3") $
        Pure (Var "n2")
    ]

{-
prim_int_add   :: T_Int64 -> T_Int64 -> T_Int64
prim_int_print :: T_Int64 -> T_Int64 -> T_Unit

f x y z =
  _2 <- prim_int_print x
  y.1 <- pure y
  z.1 <- pure z
  pure y.1

main =
  og1 <- pure 1
  og2 <- pure 2
  og3 <- pure 3
  og4 <- pure 4
  og5 <- pure 5

  k1 <- pure og1
  k2 <- pure k1
  k3 <- pure k2
  _1 <- prim_int_print k2

  l1 <- f og2 og3 og4
  m1 <- prim_int_add l1 og5

  r1 <- case m1 of
    1 @ alt1 ->
      a1 <- pure alt1
      pure a1
    2 @ alt2 ->
      og6 <- pure 6
      pure og6
    #default @ alt3 ->
      og7 <- pure 7
      pure og7

  pure r1

-}

simpleValues :: Exp 'Prg
simpleValues =
  Program
    -- type signatures of external functions must be provided at the top of the module
    [ External "prim_int_add" (TySimple T_Int64) [TySimple T_Int64, TySimple T_Int64] False
    , External "prim_int_print" (TySimple T_Unit)   [TySimple T_Int64, TySimple T_Int64] True
    ]
    [ Def "f" ["x", "y", "z"] $
        Bind (App "prim_int_print" ["x"]) (BVar "_2") $
        Bind (Pure (Var "y")) (BVar "y.1") $
        Bind (Pure (Var "z")) (BVar "z.1") $
        Pure (Var "y.1")

    , Def "main" [] $
        Bind (Pure (Val (VPrim (SInt64 1)))) (BVar "og1") $
        Bind (Pure (Val (VPrim (SInt64 2)))) (BVar "og2") $
        Bind (Pure (Val (VPrim (SInt64 3)))) (BVar "og3") $
        Bind (Pure (Val (VPrim (SInt64 4)))) (BVar "og4") $
        Bind (Pure (Val (VPrim (SInt64 5)))) (BVar "og5") $

        Bind (Pure (Var "og1")) (BVar "k1") $
        Bind (Pure (Var "k1")) (BVar "k2") $
        Bind (Pure (Var "k2")) (BVar "k3") $
        Bind (App "prim_int_print" ["k2"]) (BVar "_1") $

        Bind (App "f" ["og2", "og3", "og4"]) (BVar "l1") $
        Bind (App "prim_int_add" ["l1", "og5"]) (BVar "m1") $

        Bind (Case "m1"
          [ Alt "alt1" (LitPat (SInt64 1)) $
              Bind (Pure (Var "alt1")) (BVar "a1") $
              Pure (Var "a1")
          , Alt "alt2" (LitPat (SInt64 2)) $
              Bind (Pure (Val (VPrim (SInt64 6)))) (BVar "og6") $
              Pure (Var "og6")
          , Alt "alt3" DefaultPat $
              Bind (Pure (Val (VPrim (SInt64 7)))) (BVar "og7") $
              Pure (Var "og7")
          ]) (BVar "r1") $

        Pure (Var "r1")
    ]
