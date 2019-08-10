{-# LANGUAGE RankNTypes, LambdaCase, GADTs #-}
module Grin.GExpToExp where

import Grin.GExp
import qualified Grin.Exp as Grin


gexpToExp :: forall ctx . Exp ctx -> Grin.Exp
gexpToExp = \case
  Program exts defs -> Grin.Program exts (gexpToExp <$> defs)
  Def     n ps body -> Grin.Def n ps $ gexpToExp body
  SApp    n ps      -> Grin.SApp n ps
  SPure   v         -> Grin.SPure v
  SStore  n         -> Grin.SStore n
  SFetch  n         -> Grin.SFetch n
  SUpdate n v       -> Grin.SUpdate n v
  Alt     c body    -> Grin.Alt c $ gexpToExp body
  ECase   n alts    -> Grin.ECase n (gexpToExp <$> alts)
  EBind   lhs v rhs -> Grin.EBind (gexpToExp lhs) v (gexpToExp rhs)
