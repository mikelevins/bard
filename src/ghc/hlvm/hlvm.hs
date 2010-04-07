module Main
  where

import Data.Bits
import Data.Word
import System.Environment

import qualified Data.Map as M
import qualified Data.Sequence as S

-------------------------------------------------
-- bard values
-------------------------------------------------

type Key = Value

type Name =  String

type Var = Name

type Category = Name

type Signature =  [Category]

data ReturnType = VoidType
                | ValueType Value
                | ValuesType [Value] deriving Show

data Function = Function Name Signature ReturnType deriving Show

type Code = [Instruction] 

type Method = (Signature, Code)

type Protocol = (String, [Function])

data Value = VoidVal
           | TrueVal
           | FalseVal
           | IntegerVal Integer
           | CharacterVal Char
           | SymbolVal Name
           | KeywordVal Name
           | RecordVal [(Name, Value)]
           | PairVal (Value, Value)
           | SequenceVal (S.Seq Value)
           | MapVal (M.Map Key Value)
           | CategoryVal Category
           | FunctionVal Function
           | MethodVal Method
           | ProtocolVal Protocol deriving Show

data Env = Env (M.Map Var Value) deriving Show
nullEnv = Env M.empty

-------------------------------------------------
-- opcodes, instructions, operations
-------------------------------------------------

-------------------------------------------------
-- opcodes
-------------------------------------------------

data Opcode = Foo
    -- basic lisp operations
            | OP_CONST -- reference to a constant
            | OP_BYTE -- reference to a constant
            | OP_VREF -- reference to a module variable
            | OP_MCALL -- a macro application
            | OP_QUOTE -- a quoted expression
            | OP_IF -- an 'if' expression
            | OP_DEF -- definition of a module variable
            | OP_LAMBDA -- calling a method constructor
            | OP_FCALL -- applying a function
            | OP_CMUT -- assigning a new value to a cell
            | OP_CC -- reference to the current continuation
    -- arithmetic operations
            | OP_MULT -- multiply
    -- constants
            | OP_VOID -- reference to void
            | OP_TRUE -- reference to true
            | OP_FALSE -- reference to false
            | OP_MINUSONE -- reference to -1
            | OP_ZERO -- reference to 0
            | OP_ONE -- reference to 1
            | OP_TWO -- reference to 2
    -- I/O operations
            | OP_SHOW -- print a value 
    -- Control operations
            | OP_HALT -- print a value 
              deriving Show

-------------------------------------------------
-- instructions
-------------------------------------------------

type Instruction = (Opcode, Args)

-------------------------------------------------
-- the virtual machine
-------------------------------------------------

type VM = (Run, Code, CC, Env, NArgs, Args)

type Run = Bool
type CC = Code
type NArgs = Int
type Arg = Value
type Args = [Arg]

fetch :: VM -> Instruction
fetch (run, code, cc, env, nargs, args) = (cc!!0)

op_mult :: Args -> Value
op_mult args =
    let (IntegerVal m) = (args!!0)
        (IntegerVal n) = (args!!1)
    in
      (IntegerVal (m*n))

execute :: Instruction -> VM -> VM
execute (OP_HALT,params) (run, code, cc, env, nargs, args)     = (False, code, cc, env, nargs, args)
execute (OP_VOID,params) (run, code, cc, env, nargs, args)     = (run, code, (drop 1 cc), env, (1+(length args)), (VoidVal:args))
execute (OP_TRUE,params) (run, code, cc, env, nargs, args)     = (run, code, (drop 1 cc), env, (1+(length args)), (TrueVal:args))
execute (OP_FALSE,params) (run, code, cc, env, nargs, args)    = (run, code, (drop 1 cc), env, (1+(length args)), (FalseVal:args)) 
execute (OP_MINUSONE,params) (run, code, cc, env, nargs, args) = (run, code, (drop 1 cc), env, (1+(length args)), ((IntegerVal (-1)):args))
execute (OP_ZERO,params) (run, code, cc, env, nargs, args)     = (run, code, (drop 1 cc), env, (1+(length args)), ((IntegerVal 0):args)) 
execute (OP_ONE,params) (run, code, cc, env, nargs, args)      = (run, code, (drop 1 cc), env, (1+(length args)), ((IntegerVal 1):args)) 
execute (OP_TWO,params) (run, code, cc, env, nargs, args)      = (run, code, (drop 1 cc), env, (1+(length args)), ((IntegerVal 2):args)) 
execute (OP_CONST,params) (run, code, cc, env, nargs, args)    = (run, code, (drop 1 cc), env, (1+(length args)), ((params!!0):args)) 
execute (OP_MULT,params) (run, code, cc, env, nargs, args)     = (run, code, (drop 1 cc), env, (1+((length args)-2)), ((op_mult args):(drop 2 args)))

hlvm :: VM -> VM
hlvm (run, code, cc, env, nargs, args) =
    if run
    then let vm = (run, code, cc, env, nargs, args)
         in hlvm (execute (fetch vm) vm)
    else (run, code, cc, env, nargs, args)

-------------------------------------------------
-- main program
-------------------------------------------------

prog_mult_test = 
    [
     (OP_TWO,[]),
     (OP_CONST,[(IntegerVal 3)]),
     (OP_MULT,[]),
     (OP_HALT,[])
    ]

main = do
       putStrLn "Bard VM v 1.0"
       putStrLn (show result)
       where
         result  = hlvm (True, prog_mult_test, prog_mult_test, nullEnv, 0, [])
