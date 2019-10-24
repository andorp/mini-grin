{-# LANGUAGE RankNTypes, LambdaCase, GADTs #-}
module Grin.GExpToExp where

import Grin.GExp
import qualified Grin.Exp as Grin


gexpToExp :: forall ctx . Exp ctx -> Grin.Exp
gexpToExp = \case
  Program  exts defs -> Grin.Program exts (gexpToExp <$> defs)
  Def      n ps body -> Grin.Def n ps $ gexpToExp body
  App      n ps      -> Grin.SApp n ps
  Pure     v         -> Grin.SPure v
  Store    n         -> Grin.SStore n
  Fetch    n         -> Grin.SFetch n
  Update   n v       -> Grin.SUpdate n v
  Alt      n c b     -> Grin.Alt n c $ gexpToExp b
  Case     n alts    -> Grin.ECase n (gexpToExp <$> alts)
  Bind     lhs v rhs -> Grin.EBind (gexpToExp lhs) v (gexpToExp rhs)
