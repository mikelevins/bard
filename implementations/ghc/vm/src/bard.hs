module Main
  where

import Data.Sequence as S
import Data.Map as M
import System (getArgs)

import Values
import Machine
import IO

-------------------------------------------------
-- main program
-------------------------------------------------

main = do
  args <- getArgs
  let programFile = (read (args!!0))::String
      program = readProgram programFile
      (exp,env) = (makeVM program)
      (exp', env') = eval (exp, env)
  putStrLn ""
  putStrLn "Bard VM v 1.0"
  putStrLn ("start: ")
  putStrLn ("    exp: "++(show exp))
  putStrLn ("    env: "++(show env))
  putStrLn ("stop: ")
  putStrLn ("    exp: "++(show exp'))
  putStrLn ("    env: "++(show env'))
  putStrLn ""

