module Main
  where

import qualified Data.Map as M

import Expressions
import Environment

-------------------------------------------------
-- the virtual machine
-------------------------------------------------

type VM = (Expression, Environment)

transition :: VM -> VM
transition (ExpNothing, nullEnvironment) = (ExpNothing, nullEnvironment)

runVM :: VM -> VM
runVM (exp, env) =
    let (exp', env') = transition (exp, env)
    in if (exp', env') == (exp, env)
       then (exp',env')
       else runVM (exp', env')
    
    
-------------------------------------------------
-- main program
-------------------------------------------------

main = do
       putStrLn "Bard VM v 1.0"
