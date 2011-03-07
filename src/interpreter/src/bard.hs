module Main
  where

import Control.Monad
import Control.Monad.STM
import Data.List as L
import Data.Map as M
import System
import Text.ParserCombinators.Parsec hiding (spaces)

import Value
import Env
import Eval
import Module
import Print
import Read

-------------------------------------------------
-- main program
-------------------------------------------------

main = do
  mmgr <- atomically initModules
  let env = standardEnv
  args <- getArgs
  let inp = (args !! 0)
  putStrLn "Bard v 1.0"
  putStrLn ""
  putStrLn ("Reading: " ++ inp)
  expr <- atomically (readExpr inp mmgr)
  putStrLn ("Evaluating: " ++ (show expr))
  val <- atomically (eval expr env mmgr)
  putStrLn ("Result: " ++ (show val))
  putStrLn ""

