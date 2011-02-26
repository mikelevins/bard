module Main
  where

import Data.Map as M
import Data.Sequence as S

import Environment
import Expression
import TestPrograms
import Value
import VM

-------------------------------------------------
-- main program
-------------------------------------------------

main = do
       putStrLn "Bard VM v 1.0"
       putStrLn ""
       -- testValuePrinters
       testEvaluators


