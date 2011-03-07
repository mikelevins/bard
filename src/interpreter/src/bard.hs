module Main
  where

import Control.Monad
import Control.Monad.STM
import Data.List as L
import Data.Map as M
import IO
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

runBatch inp = do
  mmgr <- atomically initModules
  let env = standardEnv
  expr <- atomically (readExpr inp mmgr)
  val <- atomically (eval expr env mmgr)
  putStrLn ("=> " ++ (show val))
  putStrLn ""

runRepl = do
  mmgr <- atomically initModules
  let env = standardEnv
  putStrLn "Bard v 1.0"
  repl env mmgr

repl env mmgr = do
  putStr "? " >> hFlush stdout
  inp <- getLine
  case inp of
    ":q" -> exit
    ":quit" -> exit
    _ -> do expr <- atomically (readExpr inp mmgr)
            val <- atomically (eval expr env mmgr)
            putStrLn ("=> " ++ (show val))
            putStrLn ""
            (repl env mmgr)

exit = do
  putStrLn ""

main = do
  mmgr <- atomically initModules
  let env = standardEnv
  args <- getArgs
  case (length args) of
    0 -> runRepl
    1 -> let inp = (args !! 0)
         in runBatch inp
    _ -> putStrLn "Bard v 1.0: ERROR: Too many arguments"

