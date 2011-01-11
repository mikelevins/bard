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
      vm = (makeVM program)
      result = runVM vm
  putStrLn "Bard VM v 1.0"
  putStrLn ("start: "++(show vm))
  putStrLn ("program: "++(show program))
  putStrLn ("stop: "++(show result))

