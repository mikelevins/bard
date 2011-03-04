module Main
  where

import Control.Monad
import Data.List as L
import System
import Text.ParserCombinators.Parsec hiding (spaces)

import Value
import Eval
import Print
import Read

-------------------------------------------------
-- main program
-------------------------------------------------

main = do
       putStrLn "Bard v 1.0"
       putStrLn ""
       args <- getArgs
       let inp = (args !! 0)
       putStrLn ("Reading: " ++ inp)
       let expr = readExpr inp
       putStrLn ("Evaluating: " ++ (show expr))
       let val = eval expr
       putStrLn ("Result: " ++ (show val))
       putStrLn ""

