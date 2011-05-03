module Bard
  where

import Data.List as L
import Data.Map as M
import IO
import System
import Text.ParserCombinators.Parsec hiding (spaces)

import Eval
import Print
import Read
import Runtime
import Value

-------------------------------------------------
-- main program
-------------------------------------------------

runBatch inp = do
  let bard = initBard
  let (expr,bard') = readExpr inp bard
  let (val, bard'') = eval expr bard
  putStrLn ("=> " ++ (show val))
  putStrLn ""

runRepl = do
  let bard = initBard
  putStrLn "Bard v 1.0"
  repl bard

repl bard = do
  putStr "? " >> hFlush stdout
  inp <- getLine
  case inp of
    ":q" -> exit
    ":quit" -> exit
    _ -> do let (expr,bard') = readExpr inp bard
            let (val, bard'') = eval expr bard
            putStrLn ("=> " ++ (show val))
            putStrLn ""
            (repl bard)

exit = do
  putStrLn ""


