{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Tutorial.Chapter01.Exercise01 where

import qualified Grin.Exp  as E (Exp(..))
import Grin.GExp (Exp(..))

{-
TODO: Write the exercise and the motivation for it
-}

convertGExpToExp :: forall ctx . Exp ctx -> E.Exp
convertGExpToExp = \case
  Program  exts defs -> E.Program exts (map convertGExpToExp defs)
  Def      n ps body -> undefined
  Pure     v         -> E.SPure v
  Store    n         -> E.SStore n
  Fetch    n         -> undefined
  Update   n v       -> undefined
  App      n ps      -> undefined
  Alt      c body    -> E.Alt c undefined
  Case     n alts    -> undefined
  Bind     lhs v rhs -> E.EBind undefined undefined undefined
