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

type OPCODE = Word8
type Instruction = [Word8]

-- VM opcodes

op_HALT = 0::Word8
op_CC = 1::Word8
op_SETCC = 2::Word8
op_APPLY = 3::Word8
op_EVAL = 4::Word8
op_CONST = 5::Word8
op_VARREF = 6::Word8
op_QUOTE = 7::Word8
op_ALTERCELL = 8::Word8
op_COND = 9::Word8
op_MAKEMETHOD = 10::Word8


-------------------------------------------------
-- the virtual machine
-------------------------------------------------

--type VM = (Run, Code, Environment, Values)


--transition :: VM -> VM
--transition (False, code, env, vals) = (False, code, env, vals)
--transition (True, [], env, vals) = (False, code, env, vals)
-- TODO: add transitions for nonempty code

--runVM :: Code -> Values
--runVM code = vals
--    where (_, _, _, vals) = transition (True, code, standardEnvironment, [])
    
-------------------------------------------------
-- main program
-------------------------------------------------

main = do
       putStrLn "Bard VM v 1.0"
