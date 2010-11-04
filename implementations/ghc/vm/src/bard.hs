module Main
  where

import Data.Bits
import Data.Word

import qualified Data.Map as M
import qualified Data.Sequence as S

import Instructions
import Values
import Primitives
import Registers

-------------------------------------------------
-- standard environment
-------------------------------------------------

nullEnvironment = Env M.empty
standardEnvironment = nullEnvironment

-------------------------------------------------
-- the virtual machine
-------------------------------------------------

type VM = (Run, Code, Environment, [Value])

transition :: VM -> VM
transition (False, code, env, vals) = (False, code, env, vals)
transition (True, [], env, vals) = (False, [], env, vals)
transition (True, (HALT:instrs), env, vals) = (False, (HALT:instrs), env, vals)


runVM :: VM -> VM
runVM (run, code, env, vals) =
    if run
    then let vm = (run, code, env, vals)
         in runVM (transition vm)
    else (run, code, env, vals)

    
-------------------------------------------------
-- main program
-------------------------------------------------

main = do
       putStrLn "Bard VM v 1.0"
