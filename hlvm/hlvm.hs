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

type Var = String

type CategoryType = String

type SignatureType =  [CategoryType]

type FunctionType =  SignatureType

type NamedFunctionType =  (String, FunctionType)

type CodeType = [Instruction] 

type MethodType = (SignatureType, CodeType)

type ProtocolType = (String, [NamedFunctionType])

data Value = Void
           | True
           | False
           | IntValue Integer
           | Character Char
           | Symbol String
           | Keyword String
           | Record [(String, Value)]
           | Sequence (S.Seq Value)
           | Map (M.Map Key Value)
           | Category CategoryType
           | Function FunctionType
           | Method MethodType
           | Protocol ProtocolType

data Env = Env (M.Map Var Value)

-------------------------------------------------
-- opcodes, instructions, operations
-------------------------------------------------
-- opcode: an 8-bit identifier for a machine operation

-- instruction: a 64-bit value. The first 8 bits form an opcode. The
-- remaining 56 bits consist of a packed array of 8-, 16-, or 32-bit
-- values whose format is determined by the leading opcode. Each field
-- of the instruction may be empty, or another opcode, or an immediate
-- value, or an offset into vm memory

-- operation: a primitive vm routine that performs some basic function
-- of the vm, such as I/O or a Lisp primitive.

-------------------------------------------------
-- opcodes
-------------------------------------------------

type Opcode = Word8

-- basic lisp operations
op_CONST = 0xF0 :: Opcode      -- reference to a constant
op_VREF = 0xF1 :: Opcode       -- reference to a module variable
op_MCALL = 0xF2 :: Opcode      -- a macro application
op_QUOTE = 0xF3 :: Opcode      -- a quoted expression
op_IF = 0xF4 :: Opcode         -- an 'if' expression
op_DEF = 0xF5 :: Opcode        -- definition of a module variable
op_LAMBDA = 0xF6 :: Opcode     -- calling a method constructor
op_FCALL = 0xF7 :: Opcode      -- applying a function
op_CMUT = 0xF8 :: Opcode       -- assigning a new value to a cell
op_CC = 0xF9 :: Opcode         -- reference to the current continuation

-- constants
op_VOID = 0xC0 :: Opcode       -- reference to void
op_TRUE = 0xC1 :: Opcode       -- reference to true
op_FALSE = 0xC2 :: Opcode      -- reference to false
op_MINUSONE = 0xC3 :: Opcode   -- reference to -1
op_ZERO = 0xC4 :: Opcode       -- reference to 0
op_ONE = 0xC5 :: Opcode        -- reference to 1
op_TWO = 0xC6 :: Opcode        -- reference to 2

-------------------------------------------------
-- instructions
-------------------------------------------------

type Instruction = Word64

opcode :: Instruction -> Opcode
opcode ins = fromIntegral (shiftR (ins .&. 0xFF00000000000000) 56)

-------------------------------------------------
-- operations
-------------------------------------------------

-- basic lisp operations

op_const k = k

op_var_ref v env = lookup v env



-------------------------------------------------
-- main program
-------------------------------------------------

main = do
       putStrLn "Bard VM v 1.0"
