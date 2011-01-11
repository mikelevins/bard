module Main
  where

import Data.Sequence as S
import Data.Map as M
import System (getArgs)

import Values
import Instructions
import Stack
import Machine
import IO

-------------------------------------------------
-- main program
-------------------------------------------------

main = do
  args <- getArgs
  let programFile = (read (args!!0))::String
      program = readProgram programFile
      (halt, code, pc, env, nargs, stack, st_depth) = (makeVM program)
      (halt', code', pc', env', nargs', stack', st_depth') = runVM (halt, code, pc, env, nargs, stack, st_depth)
  putStrLn ""
  putStrLn "Bard VM v 1.0"
  putStrLn ("program: "++(show program))
  putStrLn ("start: ")
  putStrLn ("     pc: "++(show pc))
  putStrLn ("  stack: "++(show stack))
  putStrLn ("stop: ")
  putStrLn ("     pc: "++(show pc'))
  putStrLn ("  stack: "++(show stack'))
  putStrLn ""

