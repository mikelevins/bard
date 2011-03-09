module Main 
    where

import System

import Bard

main = do
  args <- getArgs
  case (length args) of
    0 -> runRepl
    1 -> let inp = (args !! 0)
         in runBatch inp
    _ -> do putStrLn "Bard v 1.0"
            putStrLn "  USAGE: bard [expression]"


