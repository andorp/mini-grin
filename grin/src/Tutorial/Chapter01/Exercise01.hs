{-# LANGUAGE RankNTypes #-}
module Tutorial.Chapter01.Exercise01 where

import qualified Grin.Exp  as E
import qualified Grin.GExp as G



convertGExpToExp :: forall ctx . G.Exp ctx -> E.Exp
convertGExpToExp = undefined
