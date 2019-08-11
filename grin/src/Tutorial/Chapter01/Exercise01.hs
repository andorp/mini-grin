{-# LANGUAGE RankNTypes, TypeFamilies, GADTs #-}
module Tutorial.Chapter01.Exercise01 where

import qualified Grin.Exp  as E
import qualified Grin.GExp as G

{-
TODO: Write the exercise and the motivation for it
-}


convertGExpToExp :: forall ctx . G.Exp ctx -> E.Exp
convertGExpToExp = undefined
