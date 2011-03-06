module Main
  where

import Control.Monad
import Control.Monad.STM
import Data.List as L
import Data.Map as M
import System
import Text.ParserCombinators.Parsec hiding (spaces)

import Value
import Eval
import Module
import Print
import Read

-------------------------------------------------
-- main program
-------------------------------------------------

testModules mtable = do
  putStrLn ""
  m <- atomically (findModule mtable "bard.core")
  putStrLn "Testing modules:" 
  putStrLn ("  found module \"bard.core\" == " ++ "#<bard module>")
  putStrLn ""

main = do
  mmgr <- atomically initModules
  --testModules mmgr
  args <- getArgs
  let inp = (args !! 0)
  putStrLn "Bard v 1.0"
  putStrLn ""
  putStrLn ("Reading: " ++ inp)
  expr <- atomically (readExpr inp mmgr)
  putStrLn ("Evaluating: " ++ (show expr))
  val <- atomically (eval expr mmgr)
  putStrLn ("Result: " ++ (show val))
  putStrLn ""

