{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings #-}
module Grin.Pretty
  ( PP(..)
  , WPP(..)
  , showWidth
  , showWide
  , prettyKeyValue
  , prettyFunction
  , keywordR
  , keyword
  , module Text.PrettyPrint.ANSI.Leijen
  ) where

import Data.Set (Set)
import Prelude hiding (exp)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Data.Set as Set


showWidth :: Int -> Doc -> String
showWidth w x = displayS (renderPretty 0.4 w x) ""

showWide :: Doc -> String
showWide = showWidth 156

-- plain wrappers ; remove colors

-- Pretty Show instance wrapper ; i.e. useful for hspec tests
newtype PP a = PP a deriving Eq
instance Pretty a => Show (PP a ) where
  show (PP a) = showWide . plain . pretty $ a

-- Wide pretty printing, useful for reparsing pretty-printed ASTs
newtype WPP a = WPP a deriving Eq
instance Pretty a => Show (WPP a) where
  show (WPP a) = showWide . plain . pretty $ a


keyword :: String -> Doc
keyword = yellow . text

keywordR :: String -> Doc
keywordR = red . text

-- generic ; used by HPTResult and TypeEnv

instance Pretty a => Pretty (Set a) where
  pretty s = encloseSep lbrace rbrace comma (map pretty $ Set.toList s)

prettyKeyValue :: (Pretty k, Pretty v) => [(k,v)] -> Doc
prettyKeyValue kvList = vsep [fill 6 (pretty k) <+> text "->" <+> pretty v | (k,v) <- kvList]

--prettyBracedList :: [Doc] -> Doc
--prettyBracedList = encloseSep lbrace rbrace comma

--prettySimplePair :: (Pretty a, Pretty b) => (a, b) -> Doc
--prettySimplePair (x, y) = pretty x <> pretty y

prettyFunction :: (Pretty a, Pretty name) => (name, (a, [a])) -> Doc
prettyFunction (name, (ret, args)) = pretty name <> align (encloseSep (text " :: ") empty (text " -> ") (map pretty $ args ++ [ret]))
