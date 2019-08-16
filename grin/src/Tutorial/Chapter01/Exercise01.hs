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
  Def      n ps body -> E.Def n ps (convertGExpToExp body)
  Pure     v         -> E.SPure v
  Store    n         -> E.SStore n
  Fetch    n         -> E.SFetch n
  Update   n v       -> E.SUpdate n v
  App      n ps      -> E.SApp n ps
  Alt      c body    -> E.Alt c (convertGExpToExp body)
  Case     n alts    -> E.ECase n (map convertGExpToExp alts)
  Bind     lhs v rhs -> E.EBind (convertGExpToExp lhs) v (convertGExpToExp rhs)
