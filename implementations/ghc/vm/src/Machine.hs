module Machine
    where

import Data.Map as M

import Values
import Instructions
import Stack

-------------------------------------------------
-- register types
-------------------------------------------------
                 
type Code = [Instruction]
type PC = Int
type ArgCount = Int

-------------------------------------------------
-- the virtual machine
-------------------------------------------------

type VM = (Bool, Code, PC, Env, ArgCount, Stack, StackDepth)

executeInstruction :: Instruction -> VM -> VM 

-- opcodes: constants

executeInstruction NOOP (halt, code, pc, env, nargs, stack, st_depth) =
    (halt, code, (pc+1), env, nargs, stack, st_depth)

executeInstruction HALT (halt, code, pc, env, nargs, stack, st_depth) =
    (True, code, pc, env, nargs, stack, st_depth)

executeInstruction NOTHING (halt, code, pc, env, nargs, stack, st_depth) =
    (halt, code, (pc+1), env, nargs, (ValNothing : stack), st_depth)

executeInstruction TRUE (halt, code, pc, env, nargs, stack, st_depth) =
    (halt, code, (pc+1), env, nargs, ((ValBoolean True) : stack), st_depth)

executeInstruction FALSE (halt, code, pc, env, nargs, stack, st_depth) =
    (halt, code, (pc+1), env, nargs, ((ValBoolean False) : stack), st_depth)

executeInstruction MINUS_ONE (halt, code, pc, env, nargs, stack, st_depth) =
    (halt, code, (pc+1), env, nargs, ((ValInteger (-1)) : stack), st_depth)

executeInstruction ONE (halt, code, pc, env, nargs, stack, st_depth) =
    (halt, code, (pc+1), env, nargs, ((ValInteger 1) : stack), st_depth)

executeInstruction TWO (halt, code, pc, env, nargs, stack, st_depth) =
    (halt, code, (pc+1), env, nargs, ((ValInteger 2) : stack), st_depth)

-- opcodes: branching
-- opcodes: function calls
-- opcodes: variables
-- opcodes: stack operations
-- opcodes: continuations

-- execution: step

stepVM :: VM -> VM
stepVM (halt, code, pc, env, nargs, stack, st_depth) =
    executeInstruction (code!!pc) (halt, code, pc, env, nargs, stack, st_depth)

-- execution: run

runVM :: VM -> VM
runVM (halt, code, pc, env, nargs, stack, st_depth) =
    if halt
       then (halt, code, pc, env, nargs, stack, st_depth)
       else
           runVM (stepVM (halt, code, pc, env, nargs, stack, st_depth))

-- initialization

makeStandardEnvironment = Env (M.empty)

makeVM :: Code -> VM
makeVM program = (False, program, 0, (makeStandardEnvironment), 0, [], 0)
