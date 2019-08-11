{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Tutorial.Chapter01.Exercise01 where

import qualified Grin.Exp  as E
import qualified Grin.GExp as G



convertGExpToExp :: forall ctx . G.Exp ctx -> E.Exp
convertGExpToExp = \case
  G.Program exts defs ->
    E.Program exts (convertGExpToExp <$> defs)
  G.Def name args body ->
    E.Def name args (convertGExpToExp body)
  G.SApp f xs ->
    E.SApp f xs
  G.SPure val ->
    E.SPure val
  G.SStore name ->
    E.SStore name
  G.SFetch name ->
    E.SFetch name
  G.SUpdate ref val ->
    E.SUpdate ref val
  G.Alt pat cont ->
    E.Alt pat (convertGExpToExp cont)
  G.ECase scrutinee cases ->
    E.ECase scrutinee (convertGExpToExp <$> cases)
  G.EBind lhs pat rhs ->
    E.EBind (convertGExpToExp lhs) pat (convertGExpToExp rhs)
