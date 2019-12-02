{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings, RecordWildCards, TypeApplications, RankNTypes #-}
{-# LANGUAGE DeriveGeneric, DeriveFunctor, StandaloneDeriving, TypeOperators, DataKinds, FlexibleInstances, TypeApplications #-}
{-# LANGUAGE DefaultSignatures, PolyKinds, ScopedTypeVariables, OverlappingInstances, FlexibleContexts #-}
{-# LANGUAGE GADTs, UndecidableInstances, ViewPatterns #-}
module Grin.Datalog where

{-
Change AST to conform datalog
  * Name for BTag pattern as like 'as' pattern
  * Name for Alt pattern
  * Remove unit literal
Adapt Csaba's HeapPointTo datalog algorithm to this AST
Run the datalog computation
Convert DataLog result to the TypeEnv result we have
Run the program transformation based on the TypeEnv

https://github.com/csabahruska/grin-datalog-hpt-experiment/blob/master/bb-grin-hpt.dl#L99

Render datalog input files in a directory
Run the datalog on that directory
Read back the result

[x] Use Generics to render facts
[x] Return Value algebra
[x] Next Instruction Algebra
[x] Save datalog program in a template directory
[x] Implement simple reachable function algorithm
[x] Fix NextInst, the ReturnInst should be the previous instruction
    of the case bind instead of the case result.
[x] Implement HPT based on Csaba's lambda/GRIN HPT
[x] Call Souffle interpreter
[x] Read Souffle result as TypeEnv
[ ] Change GADT to represent the node pattern problem

Later
Generate sqlite database and run souffle on that
Read sqlite result back
-}

import Grin.Examples
import Grin.GExpToExp
import Data.Functor.Foldable as Foldable
import System.IO
import Control.Monad
import Control.Monad.Reader
import Data.List (intercalate, groupBy)
import Grin.Examples
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import System.FilePath ((</>))
import System.Process
import Text.Printf
import Data.String (fromString)
import Data.List.Split
import Data.Maybe (catMaybes, mapMaybe)
import Control.Comonad
import Control.Comonad.Cofree

import qualified Data.Text as T
import qualified Grin.Exp   as G
import qualified Grin.Value as G
import qualified Grin.TypeEnv.Result as G
import qualified Data.Foldable
import qualified Data.Map as Map

import Debug.Trace


newtype Variable    = Variable G.Name deriving (Eq, Show, Ord)
newtype Function    = Function G.Name deriving (Eq, Show, Ord)
newtype Tag         = Tag G.Name deriving (Eq, Show, Ord)
newtype Literal     = Literal String deriving (Eq, Show, Ord)
newtype SimpleType  = SimpleType String deriving (Eq, Show, Ord)
newtype Number      = Number Int deriving (Eq, Show, Ord)
newtype Boolean     = Boolean Bool deriving (Eq, Show, Ord)
data    CodeName    = CVar !Variable | FName !Function deriving (Eq, Show, Ord)

data CodeFact
  = External       { f            :: !Function, effectful  :: !Boolean,   ret :: !SimpleType }
  | ExternalParam  { f            :: !Function, i          :: !Number,    st  :: !SimpleType }
  | Move           { result       :: !Variable, value      :: !Variable                           }
  | LitAssign      { result       :: !Variable, simpleType :: !SimpleType, literal    :: !Literal  }
  | Node           { result_node  :: !Variable, t          :: !Tag                                }
  | NodeArgument   { result_node  :: !Variable, i          :: !Number,    item        :: !Variable }
  | Fetch          { result       :: !Variable, value      :: !Variable                           }
  | Store          { result       :: !Variable, value      :: !Variable                           }
  | Update         { result       :: !Variable, target     :: !Variable,  value       :: !Variable }
  | Call           { call_result  :: !Variable, f          :: !Function                           }
  | CallArgument   { call_result  :: !Variable, i          :: !Number,    value       :: !Variable }
  | NodePattern    { node         :: !Variable, t          :: !Tag,       input_value :: !Variable }
  | NodeParameter  { node         :: !Variable, i          :: !Number,    parameter   :: !Variable }
  | Case           { case_result  :: !Variable, scrutinee  :: !Variable                           }
  | Alt            { case_result  :: !Variable, alt_value  :: !Variable,  t :: !Tag                }
  | AltLiteral     { case_result  :: !Variable, alt_value  :: !Variable,  l           :: !Literal  }
  | AltDefault     { case_result  :: !Variable, alt_value  :: !Variable                           }
  | ReturnValue    { n            :: !CodeName, value      :: !Variable                           }
  | FirstInst      { n            :: !CodeName, result     :: !Variable                           }
  | NextInst       { prev         :: !Variable, next       :: !Variable                           }
  | FunctionParameter { f :: !Function, i :: !Number, parameter :: !Variable }
  | AltParameter      { case_result :: !Variable, t :: !Tag, i :: !Number, parameter :: !Variable }
  deriving (Eq, Show, Ord, Generic)

class ToFact t where
  toFact :: t -> Fact

  default toFact :: (Generic t, GToFact (Rep t)) => t -> Fact
  toFact = gFactToFact . gToFact . from

instance ToFact CodeFact

class FromFact t where
  fromFact :: String -> [String] -> Maybe t

class Tags t where
  constructorTags :: Proxy t -> [String]

type Fact = (String, [Param])
type DL = ReaderT ([Fact] -> IO ()) IO

data Param
  = S String
  | N G.Name
  | I Int
  deriving Show

test :: IO ()
test = do
  programToFactsM "./datalog/facts" $ gexpToExp sumSimple
  invokeSouffle "./datalog/facts" "./datalog/out" "./datalog/analysis/dead-code.dl"
--  print =<< readCSVFile @DeadCode "DeadVariable"  "./datalog/out/DeadVariable.csv"
--  print =<< readCSVFile @DeadCode "DeadParameter" "./datalog/out/DeadParameter.csv"

emit :: (ToFact fact, Show fact) => [fact] -> DL ()
emit l = do
  f <- ask
--  liftIO $ print l
  liftIO $ f $ fmap toFact l

logWriteFile :: String -> String -> IO ()
logWriteFile fname str = do
  putStrLn $ "  " ++ fname
  writeFile fname str

programToDatalogM :: String -> G.Exp -> IO ()
programToDatalogM fname prg = do
  h <- openFile fname WriteMode
  runReaderT (convertProgram prg) $ hPutStr h . toDatalog
  hFlush h
  hClose h

programToFactsM :: FilePath -> G.Exp -> IO ()
programToFactsM dir prg = do
  let factNames = constructorTags @CodeFact Proxy
  files <- forM factNames $ \fname -> do
    let filename = fname ++ ".facts"
    putStrLn $ "\t" ++ filename
    h <- openFile (dir </> filename) WriteMode
    pure (filename, h)

  let fileMap = Map.fromList files
      writeFact (f, str) = do
        hPutStr (fileMap Map.! f) str

  runReaderT (convertProgram prg) $ mapM_ writeFact . toFacts

  forM_ (Map.elems fileMap) $ \h -> do
    hFlush h
    hClose h

invokeSouffle :: FilePath -> FilePath -> FilePath -> IO ()
invokeSouffle factsDir outDir datalogFile =
  callCommand $ printf "souffle -F%s -D%s %s" factsDir outDir datalogFile

toDatalog :: [Fact] -> String
toDatalog = unlines . map prettyFact where
  prettyFact :: Fact -> String
  prettyFact (n, args) = n ++ "(" ++ intercalate ", " (map showParam args) ++ ")."

  showParam :: Param -> String
  showParam = \case
    S s -> show s
    I i -> show i
    N n -> G.nameString n

toFacts :: [Fact] -> [(String, String)]
toFacts = map prettyFacts . Map.toList . Map.unionsWith (++) . map (\(f,a) -> Map.singleton f [a]) where
  factEq a b = fst a == fst b

  prettyFacts :: (String, [[Param]]) -> (String, String)
  prettyFacts (fname, l) = (fname ++ ".facts", unlines [intercalate "\t" (map showParam a) | a <- l])

  showParam :: Param -> String
  showParam = \case
    S s -> s
    I i -> show i
    N n -> G.nameString n

convertProgram :: G.Exp -> DL ()
convertProgram exp = do
  para (seqAlg [recurseAlg, emitAlg]) exp
  calcReturnValues exp
  void $ para nextInstAlg exp

seqAlg :: [G.ExpF (G.Exp, DL ()) -> DL ()] -> G.ExpF (G.Exp, DL ()) -> DL ()
seqAlg algs exp = mapM_ ($ exp) algs

recurseAlg :: G.ExpF (G.Exp, DL ()) -> DL ()
recurseAlg = mapM_ snd

simpleValueToFact :: G.Name -> G.SimpleValue -> CodeFact
simpleValueToFact v sv =
  (\(ty, value) ->
    LitAssign
      { result      = Variable v
      , simpleType  = ty
      , literal     = Literal value
      })
  $ case sv of
      G.SInt64  i -> (stToDatalogST G.T_Int64,  show i)
      G.SWord64 w -> (stToDatalogST G.T_Word64, show w)
      G.SFloat  f -> (stToDatalogST G.T_Float,  show f)
      G.SBool   b -> (stToDatalogST G.T_Bool,   show b)
      G.SChar   c -> (stToDatalogST G.T_Char,   show c)

gtagToDtag :: G.Tag -> Tag
gtagToDtag (G.Tag tt name) = Tag $ G.mkName (renderTagType tt) <> name
  where
    renderTagType :: G.TagType -> String
    renderTagType G.C = "C"
    renderTagType G.F = "F"
    renderTagType (G.P m) = "P-" ++ show m ++ "-"

dtagToGtag :: Tag -> G.Tag
dtagToGtag (Tag tag) = case G.nameString tag of
  'C':name -> G.Tag G.C (G.mkName name)
  'F':name -> G.Tag G.F (G.mkName name)
  'P':_ -> case T.splitOn "-" $ G.getName tag of
    ["P",(read . show) -> m, name] -> G.Tag (G.P m) (G.NM name)
    _ -> error $ show tag

emitAlg :: G.ExpF (G.Exp, DL ()) -> DL ()
emitAlg = \case
  G.ProgramF externals defs -> do
    convertExternals externals

  -- f param0 param1 = ...
  -- .decl FunctionParameter(f:Function, i:number, parameter:Variable)
  G.DefF name args body -> do
    emit $ zipWith
      (\n a -> FunctionParameter
        { f         = Function name
        , i         = Number n
        , parameter = Variable a
        })
      [0..]
      args

  -- result <- pure value
  -- .decl Move(result:Variable, value:Variable)
  G.EBindF (G.SPure (G.Var val), lhs) (G.BVar res) (_, rhs) -> do
    emit [Move { result = Variable res, value = Variable val }]

  -- result <- pure 1
  -- .decl LitAssign(result:Variable, l:Literal)
  G.EBindF (G.SPure (G.Val (G.VPrim s)), lhs) (G.BVar var) (_, rhs) -> do
    emit [ simpleValueToFact var s ]

  -- result_node <- pure (Ctag item0 item1)
  -- .decl Node(result_node:Variable, t:Tag)
  -- .decl NodeArgument(result_node:Variable, i:number, item:Variable)
  G.EBindF ((G.SPure (G.Val (G.VNode (G.Node tag items)))), lhs) (G.BVar res) (_, rhs) -> do
    emit $ (Node { result_node = Variable res, t = gtagToDtag tag })
         : zipWith (\n v ->
            NodeArgument
              { result_node = Variable res
              , i           = Number n
              , item        = Variable v
              })
            [0..]
            items

  -- example: result <- fetch value
  -- .decl Fetch(result:Variable, value:Variable)
  G.EBindF (G.SFetch val, lhs) (G.BVar res) (_, rhs) -> do
    emit [Fetch { result = Variable res, value = Variable val }]

  -- example: result <- store value
  -- .decl Store(result:Variable, value:Variable)
  G.EBindF (G.SStore val, lhs) (G.BVar res) (_, rhs) -> do
    emit [Store { result = Variable res, value = Variable val }]

  -- example: result <- update target value
  -- .decl Update(result:Variable, target:Variable, value:Variable)
  G.EBindF (G.SUpdate tar val, lhs) (G.BVar res) (_, rhs) -> do
    emit [Update { result = Variable res, target = Variable tar, value = Variable val }]

  -- call_result <- f value0 value1
  -- .decl Call(call_result:Variable, f:Function)
  -- .decl CallArgument(call_result:Variable, i:number, value:Variable)
  G.EBindF (G.SApp{},_) (G.BNodePat{}) _ -> error "Invalid AST."
  G.EBindF (G.SApp fun args, lhs) (G.BVar res) (_, rhs) -> do
    emit  $ (Call { call_result = Variable res, f = Function fun })
          : (zipWith
              (\n a -> CallArgument
                { call_result = Variable res
                , i           = Number n
                , value       = Variable a
                }))
              [0..]
              args

  -- bind pattern
  -- node@(Ctag param0 param1) <- pure input_value
  -- .decl NodePattern(node:Variable, t:Tag, input_value:Variable)
  -- .decl NodeParameter(node:Variable, i:number, parameter:Variable)
  G.EBindF (G.SPure (G.Var inp_val), lhs) (G.BNodePat nd tag pms) (_, rhs) -> do
    emit  $ (NodePattern
              { node        = Variable nd
              , t           = gtagToDtag tag
              , input_value = Variable inp_val
              })
          : zipWith
              (\n p ->
                NodeParameter
                  { node      = Variable nd
                  , i         = Number n
                  , parameter = Variable p
                  })
              [0..]
              pms

  -- case + alt
  -- example:
  -- case_result <- case scrut of
  --   alt_value@(Ctag param0 param1) -> basic_block_name arg0 arg1
  -- .decl Case(case_result:Variable, scrutinee:Variable)
  -- .decl Alt(case_result:Variable, alt_value:Variable, t:Tag)
  -- .decl AltParameter(case_result:Variable, t:Tag, i:number, parameter:Variable)
  -- .decl AltLiteral(case_result:Variable, alt_value:Variable, l:Literal)
  -- .decl AltDefault(case_result:Variable, alt_value :: Variable)
  G.EBindF (G.ECase scr alts, lhs) (G.BVar cs_res) (_, rhs) -> do
    emit $ concat
      $ [Case { case_result = Variable cs_res
              , scrutinee   = Variable scr
              }]
      : (flip map alts $ \case
          G.Alt n (G.NodePat tag args) _ ->
              Alt { case_result = Variable cs_res
                  , alt_value = Variable n
                  , t = gtagToDtag tag
                  }
            : zipWith (\j a ->
                AltParameter
                  { case_result = Variable cs_res
                  , t           = gtagToDtag tag
                  , i           = Number j
                  , parameter   = Variable a
                  })
              [0..]
              args
          G.Alt n (G.LitPat (G.SInt64 i)) _ ->
            [ AltLiteral
                { case_result = Variable cs_res
                , alt_value = Variable n
                , l = Literal (show i)
                }
            ]
          G.Alt n (G.LitPat (G.SBool b)) _ ->
            [ AltLiteral
                { case_result = Variable cs_res
                , alt_value = Variable n
                , l = Literal (show b)
                }
            ]
          G.Alt n G.DefaultPat _ ->
            [ AltDefault { case_result = Variable cs_res, alt_value = Variable n} ]
          _ -> error "Case, Alt, non expected constructor."
        )

  _ -> pure ()


calcReturnValues :: G.Exp -> DL ()
calcReturnValues = emit . snd . histo returnValueAlg

returnValueAlg :: G.ExpF (Cofree G.ExpF (Maybe G.Name, [CodeFact])) -> (Maybe G.Name, [CodeFact])
returnValueAlg = \case
  G.SPureF (G.Var name) -> (Just name, [])
  G.SPureF val          -> (Nothing, [])
  G.EBindF ((_, lhs) :< G.ECaseF _ alts) (G.bpatVar -> v) (extract -> (returnValue, rhs))
    -> let returnValues = mapMaybe (fst . extract) alts
       in ( returnValue
          , map (\r -> NextInst { prev = Variable r, next = Variable v }) returnValues ++ lhs ++ rhs)
  G.EBindF (extract -> (_, lhs)) _ (extract -> (returnValue, rhs))
    -> (returnValue, lhs ++ rhs)
  G.AltF codeName _ (extract -> (Just returnValue, body)) ->
    (Just returnValue, (ReturnValue { n = CVar $ Variable codeName, value = Variable returnValue }):body)
  G.DefF codeName _ (extract -> (Just returnValue, body)) ->
    (Nothing, (ReturnValue { n = FName $ Function codeName, value = Variable returnValue }):body)
  rest -> (,) Nothing $ concat $ fmap (snd . extract) $ rest

-- | The next instrument makes a chain of variable associations, between binds
nextInstAlg :: G.ExpF (G.Exp, DL (Maybe G.Name)) -> DL (Maybe G.Name)
nextInstAlg = \case
  G.DefF codeName _ (G.EBind _ (G.bpatVar -> v) _, body) -> do
    void body
    emit [ FirstInst { n = FName $ Function codeName, result = Variable v } ]
    pure Nothing
  G.AltF n _ (G.EBind _ (G.bpatVar -> v) _, body) -> do
    void body
    emit [ NextInst { prev = Variable n, next = Variable v } ]
    pure $ Just n
  G.ECaseF v alts -> do
    mnis <- mapM snd alts
    forM mnis $ mapM_ $ \ni -> do
      -- TODO: Improve
      emit [ NextInst { prev = Variable v, next = Variable ni } ]
    pure Nothing
  --G.EBindF (G.ECase{..}, lhs) _ (_,rhs) -> do
  G.EBindF (elhs, lhs) (G.bpatVar -> v) (_, rhs) -> do
    void lhs
    mni <- rhs
    forM_ mni $ \ni -> do
      emit [ NextInst { prev = Variable v, next = Variable ni } ]
    -- pure $ Just v
    pure $ case elhs of
      G.ECase{} -> Nothing
      _         -> Just v
  rest -> do
    mapM_ snd rest
    pure Nothing

convertExternals :: [G.External] -> DL ()
convertExternals
  = emit . concatMap (\(G.External n rt pts e) ->
      (External
          { f         = Function n
          , effectful = Boolean e
          , ret       = asDatalogSimpleType rt
          }
      ) : map (\(t,j)
                -> ExternalParam
                    { f   = Function n
                    , i   = Number j
                    , st  = asDatalogSimpleType t
                    }) (pts `zip` [0..]))
  where
    asDatalogSimpleType :: G.Ty -> SimpleType
    asDatalogSimpleType = \case
      G.TySimple st -> stToDatalogST st
      _ -> error "asDatalogSimpleType: None handled"

stToDatalogST :: G.SimpleType -> SimpleType
stToDatalogST = SimpleType . \case
  G.T_Int64   -> "Int64"
  G.T_Word64  -> "Word64"
  G.T_Float   -> "Float"
  G.T_Bool    -> "Bool"
  G.T_Char    -> "Char"
  G.T_Unit    -> "Unit"
  other -> error $ "stToDatalogST: None handled case: " ++ show other

-- * Generics

data GFact
  = FactHead String GFact
  | FArgs    [GFact]
  | FactArg  Param
  deriving Show

gFactToParams :: GFact -> [Param]
gFactToParams = \case
  FactHead _ g  -> gFactToParams g
  FArgs as      -> concatMap gFactToParams as
  FactArg p     -> [p]

gFactToFact :: GFact -> Fact
gFactToFact (FactHead n g) = (n, gFactToParams g)

class GToFact t where
  gToFact :: t a -> GFact

instance (GToFact a, GToFact b) => GToFact (a :+: b) where
  gToFact (L1 l) = gToFact l
  gToFact (R1 r) = gToFact r
instance (GToFact a, GToFact b) => GToFact (a :*: b) where
  gToFact (a :*: b) = case gToFact b of
    FArgs as -> FArgs (gToFact a : as)
    other    -> FArgs [gToFact a, other]

instance (KnownSymbol n, GToFact a) => GToFact (M1 C (MetaCons n f b) a) where
  gToFact (M1 x) = FactHead (symbolVal (Proxy @n)) $ gToFact x

instance (GToFact a) => GToFact (M1 C (MetaData n m p nt) a) where
  gToFact (M1 x) = gToFact x

instance (GToFact a) => GToFact (M1 C (MetaSel mn su ss ds) a) where
  gToFact (M1 x) = gToFact x

instance (GToFact a) => GToFact (M1 c i a) where
  gToFact (M1 x) = gToFact x

instance (IsParam a) => GToFact (K1 i a) where
  gToFact (K1 x) = FactArg $ toParam x

-- TODO: Use generics
instance Tags CodeFact where
  constructorTags _ =
    [ "External"
    , "ExternalParam"
    , "Move"
    , "LitAssign"
    , "SimpleType"
    , "Node"
    , "NodeArgument"
    , "Fetch"
    , "Store"
    , "Update"
    , "Call"
    , "CallArgument"
    , "NodePattern"
    , "NodeParameter"
    , "Case"
    , "Alt"
    , "AltParameter"
    , "AltLiteral"
    , "AltDefault"
    , "ReturnValue"
    , "FirstInst"
    , "NextInst"
    , "FunctionParameter"
    ]

class IsParam t where toParam :: t -> Param
instance IsParam Variable where toParam (Variable n) = N n
instance IsParam Function where toParam (Function n) = N n
instance IsParam Tag      where toParam (Tag n)      = N n
instance IsParam Literal  where toParam (Literal l)  = S l
instance IsParam Number   where toParam (Number i)   = I i
instance IsParam Boolean  where toParam (Boolean b)  = I (if b then 1 else 0)
instance IsParam SimpleType where toParam (SimpleType s) = S s
instance IsParam CodeName where
  toParam = \case
    CVar  v -> toParam v
    FName f -> toParam f

-- * Read CSV file

readCSVFile :: (FromFact t) => String -> FilePath -> IO [t]
readCSVFile factKind
  = fmap (catMaybes . fmap (fromFact factKind . splitOn ",") . lines) . readFile
