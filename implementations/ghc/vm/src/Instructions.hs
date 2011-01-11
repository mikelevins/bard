module Instructions
    where

data Instruction = NOOP
                 | HALT
                 -- constants
                 | NOTHING
                 | TRUE
                 | FALSE
                 | MINUS_ONE
                 | ONE
                 | TWO
                 -- branching
                 | JUMP
                 | FJUMP
                 | TJUMP
                 -- functions and methods
                 | SAVE
                 | RETURN
                 | CALLJ
                 | ARGS
                 | VARARGS
                 | FUNCTION
                 | METHOD
                 | ADDPRIM
                 | PRIM
                 | APPLY
                 -- variables
                 | LVAR
                 | LSET
                 | MVAR
                 | MSET
                 -- stack operations
                 | POP
                 -- continuations
                 | SETCC
                 | CC
                   deriving Show
