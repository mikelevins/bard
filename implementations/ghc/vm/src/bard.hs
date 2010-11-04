module Main
  where

import Data.Bits
import Data.Word

import qualified Data.Map as M
import qualified Data.Sequence as S

-------------------------------------------------
-- bard values
-------------------------------------------------

-------------------------------------------------
-- hlvm operations
-------------------------------------------------

data Instruction = HALT deriving (Show, Read)

type Name = String

type Arglist = [Name]

type Names = [Name]
type ModuleName = Name
type ExportedNames = [Name]
type ImportedNames = (M.Map ModuleName Names)

data Value = NothingVal
           | IntegerVal Int
           | FloatVal Float
           | NameVal String
           | SeqVal [Value]
           | MapVal (M.Map Value Value)
           | MethodVal Arglist Code
           | ModuleVal Names ExportedNames ImportedNames deriving Show


-------------------------------------------------
-- VM register types
-------------------------------------------------

type Run = Bool
type Code = [Instruction]
data Environment = Env (M.Map Name Value) deriving Show
nullEnvironment = Env M.empty
type Values = [Value]

-------------------------------------------------
-- the virtual machine
-------------------------------------------------

type VM = (Run, Code, Environment, Values)

transition :: VM -> VM
transition (False, code, env, vals) = (False, code, env, vals)
transition (True, [], env, vals) = (False, [], env, vals)
transition (True, (HALT:instrs), env, vals) = (False, (HALT:instrs), env, vals)


--runVM :: Code -> Values
--runVM code = vals
--    where (_, _, _, vals) = transition (True, code, standardEnvironment, [])
    
-------------------------------------------------
-- main program
-------------------------------------------------

main = do
       putStrLn "Bard VM v 1.0"
