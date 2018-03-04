module Levy.CodeGen where

import Levy.AST
import Control.Monad.State.Lazy (State, evalState, get, put)

data Env = Env { symbolTable :: Map String Int
                 , nextCounterCell :: Int }
fresh :: State Env Int
fresh = do
  env <- get
  let cs = nextCounterCell env
  put (env { nextCounterCell = cs + 1 })
  return cs

codeGen :: LevyProgram -> [Char]
codeGen _ = []