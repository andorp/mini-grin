{-# LANGUAGE LambdaCase, GADTs, ConstraintKinds, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, FlexibleContexts #-}
module Grin.Pipeline.Base where

import Control.Monad.State.Class (MonadState(..), modify, gets)
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.IO.Class (MonadIO(..))
import Grin.Exp
import Grin.GExpToExp (gexpToExp)
import Grin.Examples (sumSimple)
import Grin.Datalog hiding (Fact, test)
import Grin.TypeEnv.Result (printTypeEnv)
import System.Directory
import Control.Monad
import Grin.Transformation.Base (ExpChanged(..), bindNormalisation)

import Grin.Analysis.BoqHPT

import qualified Grin.Transformation.Base as T (Transformation(..), Analysis(..))
import qualified Grin.Transformation.DeadParameterElimination as Transformation
import qualified Grin.Transformation.DeadVariableElimination  as Transformation


data Transformation where
  Transformation
    :: ( T.Transformation t
       , T.Analysis (T.AnalysisOf t)
       , FromFact (T.Fact (T.AnalysisOf t))
       , Show (T.Result (T.AnalysisOf t))
       )
    => t -> Transformation

data Analysis b where
  Analysis
    :: ( T.Analysis a
       , FromFact (T.Fact a)
       , Show (T.Result a)
       )
    => a
    -> (T.Result a -> b)
    -> Analysis b

data PipelineState = PipelineState
  { pipelineStep        :: Int
  , pipelineOptStep     :: Int
  , indent              :: Int
  , expression          :: Exp
  , expressionChanged   :: [ExpChanged] -- Set of changes
  }

initPipelineState :: Exp -> PipelineState
initPipelineState e = PipelineState
  { pipelineStep        = 0
  , pipelineOptStep     = 0
  , indent              = 0
  , expression          = e
  , expressionChanged   = []
  }

type Ctx m = (Monad m, MonadIO m)

newtype Pipeline m a = Pipeline { runPipeline :: StateT PipelineState m a }
  deriving (Functor, Applicative, Monad, MonadState PipelineState, MonadIO)

test :: IO ()
test = do
  --e <- execute (gexpToExp sumSimple) $ do
  --      transformation $ Transformation Transformation.DeadVariableElimination
  --      getExpression
  printGrin (gexpToExp sumSimple)
  execute (gexpToExp sumSimple) $ do
    join $ analysis $ Analysis BoqHPT (\(BoqHPTResult t) -> liftIO $ printTypeEnv t)
  --printGrin test2
  --execute test2 $ do
  --  analysis $ Analysis CreatedBy

execute :: Ctx m => Exp -> Pipeline m a -> m a
execute e = fmap fst . flip runStateT (initPipelineState e) . runPipeline

getExpression :: Ctx m => Pipeline m Exp
getExpression = gets expression

transformation :: Ctx m => Transformation -> Pipeline m ()
transformation (Transformation t) = bracket $ do
  modify $ \s -> s { expressionChanged = [] }
  i <- analysisImpl (T.analysisOf t)
  e <- gets expression
  let me = T.transform t i e
  forM_ me $ \(e0,changes) -> do
    let e1 = if NewBlock `elem` changes
              then bindNormalisation e0
              else e0
    modify $ \s -> s { expression = e1, expressionChanged = changes }

analysis :: Ctx m => Analysis b -> Pipeline m b
analysis (Analysis a k) = bracket $ do
  modify $ \s -> s { expressionChanged = [] }
  r <- analysisImpl a
  pure (k r)

bracket :: Ctx m => Pipeline m a -> Pipeline m a
bracket p = do
  i <- gets indent
  modify $ \s -> s { indent = max 15 (indent s + 1) }
  x <- p
  modify $ \s -> s { indent = i }
  pure x

analysisImpl
  :: forall m a
   . (Ctx m, T.Analysis a, FromFact (T.Fact a))
  => a
  -> Pipeline m (T.Result a)
analysisImpl a = do
  s <- gets pipelineOptStep
  let dir = "./.compilation/" ++ show s
  e <- gets expression
  liftIO $ do
    createDirectoryIfMissing True dir
    createDirectoryIfMissing True $ dir ++ "/facts"
    createDirectoryIfMissing True $ dir ++ "/dl-result"
    programToFactsM (dir ++ "/facts") e
    invokeSouffle
      (dir ++ "/facts")
      (dir ++ "/dl-result")
      ("./datalog/analysis/" ++ T.datalogProgram a ++ ".dl")
    facts <- forM (T.outputFactFiles a) $ \fn -> do
      readCSVFile @(T.Fact a) fn (dir ++ "/dl-result/" ++ fn ++ ".csv")
    pure $ T.fromFacts a $ concat facts
