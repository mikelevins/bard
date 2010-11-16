module Main
  where

import Numeric (readInt)
import System (getArgs)

import qualified Data.Map as M

import Expressions
import Environment

-------------------------------------------------
-- the virtual machine
-------------------------------------------------

type VM = (Expression, Environment)

isNormalForm :: Expression -> Environment -> Bool
isNormalForm ExpNothing _ = True
isNormalForm (ExpInt int) _ = True
isNormalForm (ExpFloat fl) _ = True
isNormalForm (ExpChar ch) _ = True
isNormalForm (ExpName nm) _ = False
isNormalForm exp env = True

reduce :: VM -> VM
reduce (ExpNothing, env) = (ExpNothing, env)
reduce ((ExpInt int), env) = ((ExpInt int), env)
reduce ((ExpFloat fl), env) = ((ExpFloat fl), env)
reduce ((ExpChar ch), env) = ((ExpChar ch), env)
reduce ((ExpName nm), env) = ((lookupName env (ExpName nm)), env)

runVM :: VM -> VM
runVM (exp, env) =
    if isNormalForm exp env
       then (exp, env)
       else
           runVM (reduce (exp, env))

-------------------------------------------------
-- test programs
-------------------------------------------------

testPrograms = [
 ExpNothing,
 (ExpInt 23),
 (ExpFloat 12.34),
 (ExpChar 'B'),
 (ExpName "nothing")]

-------------------------------------------------
-- main program
-------------------------------------------------

main = do
  args <- getArgs
  let index = read (args!!0)
      prog = testPrograms!!index
      (result, _) = (runVM (prog, standardEnvironment))
  putStrLn "Bard VM v 1.0"
  putStrLn ("in: "++(show prog))
  putStrLn ("out: "++(show result))

