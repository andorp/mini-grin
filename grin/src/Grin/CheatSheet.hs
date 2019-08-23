{-# LANGUAGE ConstraintKinds #-}
module Grin.CheatSheet where

import qualified Grin.Value
import qualified Grin.Exp
import qualified Grin.GExp
import qualified Grin.Interpreter.Env
import qualified Grin.Interpreter.Store
import qualified Tutorial.Chapter01.Exercise02
import qualified Grin.Interpreter.Abstract.Base


-- * Prelude
--
-- $prelude
-- === Introduction:
-- https://github.com/grin-compiler/presentations/blob/master/2018/haskell-exchange-2018/Grin-HaskellX2018.pdf
--
-- === What is GRIN
-- GRIN stands for Graph Reduction Intermediate Notation, and is
-- a compiler back end for functional languages. As its name
-- suggests, GRIN can be used to express graph reduction semantics
-- and hence can be used to compile functional languages.
--
-- In GRIN, a node in the graph is represented as a C-stlye struct.
-- The Heap can only contain Node values and nothing else.
-- These Node values stored on the Heap are the nodes of the functional program's
-- graph. The reduction of this graph is done through the primitive
-- heap operations of GRIN (store, fetch, update).
--
-- === Could you tell it again?
-- GRIN is a very simple C like language:
--     * C without pointer arithmetic
--     * C without loops
--     * C without types
--     * Structs called Nodes have a fixed structure, a tag and some non-named arguments
--     * Switch can match on Nodes
--     * GRIN codes must be well-formed
--
-- === How to run the tests
-- > stack ghci --test
-- > :l grin/test/Tutorial/Chapter01/Exercise01Spec.hs
-- > hspec spec
--
-- OR
--
-- > stack test --file-watch

-- * Chapter 01 / Exercise 01
--
-- $c01e01
-- === Original Syntax
-- https://nbviewer.jupyter.org/github/grin-compiler/grin/blob/master/papers/boquist.pdf#page=44
--
-- === Sequencing of GRIN opeartions
-- https://nbviewer.jupyter.org/github/grin-compiler/grin/blob/master/papers/boquist.pdf#page=46
--
-- === Link GADT syntax
-- https://en.wikibooks.org/wiki/Haskell/GADT#GADTs
--
-- === Exercise
-- Implement 'Tutorial.Chapter01.Exercise01.convertGExpToExp'
-- TODO: Program Graph of SumSimple

type Name         = Grin.Value.Name
type Tag          = Grin.Value.Tag
type SimpleValue  = Grin.Value.SimpleValue
type Node         = Grin.Value.Node
type Value        = Grin.Value.Value
type VarOrValue   = Grin.Value.VarOrValue

type External = Grin.Exp.External
type CPat     = Grin.Exp.CPat
type BPat     = Grin.Exp.BPat

type ExpCtx   = Grin.GExp.ExpCtx
type GExp     = Grin.GExp.Exp

type Exp = Grin.Exp.Exp


-- * Chapter 01 / Exercise 02
--
-- $c01e02
-- === Semantics
--     * https://nbviewer.jupyter.org/github/grin-compiler/grin/blob/master/papers/boquist.pdf#page=52
--     * https://nbviewer.jupyter.org/github/grin-compiler/grin/blob/master/papers/boquist.pdf#page=53
--     * https://nbviewer.jupyter.org/github/grin-compiler/grin/blob/master/papers/boquist.pdf#page=55
--
-- === Exercise
-- Read the module for exercises "Tutorial.Chapter01.Exercise02"

type Env   = Grin.Interpreter.Env.Env
type Store = Grin.Interpreter.Store.Store
type C01E02_Value = Tutorial.Chapter01.Exercise02.Value
type C01E02_Node= Tutorial.Chapter01.Exercise02.Node
type C01E02_SValue = Tutorial.Chapter01.Exercise02.SValue
type C01E02_Address = Tutorial.Chapter01.Exercise02.Address
type C01E02_InterpretExternal = Tutorial.Chapter01.Exercise02.InterpretExternal
type C01E02_Functions = Tutorial.Chapter01.Exercise02.Functions
type C01E02_Definitional = Tutorial.Chapter01.Exercise02.Definitional

-- * Interlude: Intermediate language to compile from Lambda Calculus to GRIN
--
-- $interlude1
--
-- === Lambda as an intermediate language
-- https://nbviewer.jupyter.org/github/grin-compiler/grin/blob/master/papers/boquist.pdf#page=65
--
-- === Code generation from Lambda
-- https://nbviewer.jupyter.org/github/grin-compiler/grin/blob/master/papers/boquist.pdf#page=64

-- * Chapter 02 / Exercise 01
--
-- $c02e1
-- === From Machines to Compositional Evaluators
-- https://plum-umd.github.io/abstracting-definitional-interpreters/#%28part._s~3aaam%29
--
-- === Exercise
-- Review the "Tutorial.Chapter02.Exercise01" module

-- * Chapter 02 / Exercise 02
--
-- $c02e02
-- === Abstracting Closures
-- https://plum-umd.github.io/abstracting-definitional-interpreters/#%28part._s~3aabstracting-closures%29
--
-- === Exercise
-- Review the "Tutorial.Chapter02.Exercise02" module and solve the exercises

type AbstractT = Grin.Interpreter.Abstract.Base.AbstractT
type Cache = Grin.Interpreter.Abstract.Base.Cache
type TypeEnv = Grin.Interpreter.Abstract.Base.TypeEnv
type T = Grin.Interpreter.Abstract.Base.T
type ST = Grin.Interpreter.Abstract.Base.ST
type Loc = Grin.Interpreter.Abstract.Base.Loc
type AbsStore = Grin.Interpreter.Abstract.Base.AbsStore
type AbsEnv = Grin.Interpreter.Abstract.Base.AbsEnv
type AbsState = Grin.Interpreter.Abstract.Base.AbsState
type C02E02_Node = Grin.Interpreter.Abstract.Base.Node

-- * Interlude: Connection between pointer analysis, type systems and abstract interpretations
--
-- $interlude2
-- The original version of this tutorial meant to use a constraint solver for solving the type
-- equations, but after I decided to give a go with the abstract interpretation.

-- * Chapter 03 / Exercise 01
--
-- $c03e01
--
-- === Exercise
-- Fill out the missing pieces in "Tutorial.Chapter03.Exercise01"

-- * Chapter 03 / Exercise 02
--
-- $c0302
--
-- === Exercise
-- Solve the exercises in "Tutorial.Chapter03.Exercise02"

-- * Epilogue: Possible futures of the Whole Program Analysis
--
-- $epilogue
-- === Discussion about:
--     * Incremental Whole Program Analysis
--     * Module Whole Program Analysis
--     * Link time optimisations
