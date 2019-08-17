{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Tutorial.Chapter01.Exercise01 where

import qualified Grin.Exp  as E (Exp(..))
import Grin.GExp (Exp(..))

{-
The GRIN is a simple language. The GRIN programs consist of
 * creating values via Pure or funtion application App
 * binding values to variables using patterns
 * branching control flow is done via case alternatives
   which selects the first from the alternatives that
   match a value in the scrutinee
 * Only structured/boxed values can be stored on a heap,
   which are called Node values
 * Operations of the heap are Store a Node value,
   Fetch a Node value from the heap, Update a Node value
   on a heap location.

Exercise:
Open the Grin.Examples module and take a look at the
three examples to build an intuition about the GRIN language
and about the GADT represented syntax.

Motivation:
It is good to have a type safe GADT representation of the
GRIN language which restricts some constructions,
but it is easier to handle a simple ADT represented
program in transformation and analysises.

Exercise:
Check the cheatsheet about the GRIN values.
Check the cheatsheet about the GRIN patterns.
Open the Grin.Exp module and check the Exp datatype.
Open the Grin.GExp module and check the GExp datatype.

Exercise:
Complete the definition above.
-}

convertGExpToExp :: forall ctx . Exp ctx -> E.Exp
convertGExpToExp = \case
  Program  exts defs -> E.Program exts (map convertGExpToExp defs)

  -- Exercise: Map the Def constructor to its E.Exp counterpart.
  Def      n ps body -> undefined

  Pure     v         -> E.SPure v

  -- Exercise: Check what kind of values can be stored on the heap?
  Store    n         -> E.SStore n

  -- Exercise: Map the Fetch constructor to its E.Exp counterpart.
  Fetch    n         -> undefined

  -- Exercise: Map the Update constructor to its E.Exp counterpart.
  Update   n v       -> undefined

  -- Exercise: Map the App constructor to its E.Exp counterpart.
  App      n ps      -> undefined

  -- Exercise: Turn the body of the alt to an E.Exp
  Alt      c body    -> E.Alt c undefined

  -- Exercise: Turn the Case constructor to its E.Exp counterpart.
  Case     n alts    -> undefined

  -- Exercise: Check what kind of syntactical construction is the Bind
  -- and convert the lhs and rhs to E.Exp, also use the pattern
  Bind     lhs pat rhs -> E.EBind undefined undefined undefined
