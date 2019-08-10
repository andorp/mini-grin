module Tutorial.Chapter01.Exercise02 where

import Grin.Exp
import Grin.Value
import Grin.Interpreter.Env
import Grin.Interpreter.Store


data Context = Context
  { externalCall :: External -> [Val] -> IO Val
  }

-- Implement the external calls for the externals used in the Examples
externalCalls :: External -> [Val] -> IO Val
externalCalls = undefined

-- Write a function that interprets the given expression. The function gets the body of the main.
interpret :: Context -> Exp -> IO Val
interpret = undefined
