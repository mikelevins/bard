module Main
  where

import Data.Array.IO 
import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import System.Environment

-------------------------------------------------
-- Instructions
-------------------------------------------------

type Opcode = Word8
type Instruction = Word64
type Operand = Word64
type Code = [Instruction]
type Heap = IOUArray Int Word64

opcode :: Instruction -> Opcode
opcode ins = fromIntegral (shiftR (ins .&. 0xFF00000000000000) 56)

instr0 :: Opcode -> Instruction
instr0 op = fromIntegral (shiftL op 56)

-- variable and stack manipulations
op_LVAR = 0xE1 :: Opcode
op_LSET = 0xE2 :: Opcode
op_GVAR = 0xE3 :: Opcode
op_GSET = 0xE4 :: Opcode
op_POP = 0xE5 :: Opcode
op_CONST = 0xE6 :: Opcode
-- branching
op_JUMP = 0xF1 :: Opcode
op_FJUMP = 0xF2 :: Opcode
op_TJUMP = 0xF3 :: Opcode
-- function call and return
op_SAVE = 0xF4 :: Opcode
op_RETURN = 0xF5 :: Opcode
op_CALLJ = 0xF6 :: Opcode
op_ARGS = 0xF7 :: Opcode

op_FN = 0xF8 :: Opcode
op_PRIM = 0xF9 :: Opcode
-- handling continuations
op_SETCC = 0xFA :: Opcode
op_CC = 0xFB :: Opcode
-- unary operations
op_CAR = 0xA1 :: Opcode
op_CDR = 0xA2 :: Opcode
op_CADR = 0xA3 :: Opcode
op_NOT = 0xA4 :: Opcode
op_LIST1 = 0xA5 :: Opcode
op_COMPILER = 0xA6 :: Opcode
op_DISPLAY = 0xA7 :: Opcode
op_WRITE = 0xA8 :: Opcode
op_RANDOM = 0xA9 :: Opcode
-- binary operations
op_ADD = 0xB1 :: Opcode
op_SUB = 0xB2 :: Opcode
op_MUL = 0xB3 :: Opcode
op_DIV = 0xB4 :: Opcode
op_REM = 0xB5 :: Opcode
op_LT =  0xB6:: Opcode
op_GT = 0xB7 :: Opcode
op_LTE = 0xB8 :: Opcode
op_GTE = 0xB9 :: Opcode
op_NEQ = 0xBA :: Opcode
op_EQL = 0xBB :: Opcode
op_CONS = 0xBC :: Opcode
op_LIST2 = 0xBD :: Opcode
op_NAME = 0xBE :: Opcode
op_EQ = 0xBF :: Opcode
op_EQUAL = 0xB0 :: Opcode
-- ternary operations
op_LIST3 = 0xD1 :: Opcode
-- constants
op_NIL = 0xC0 :: Opcode
op_TRUE = 0xC1 :: Opcode
op_FALSE = 0xC2 :: Opcode
op_MINUSONE = 0xC3 :: Opcode
op_ZERO = 0xC4 :: Opcode
op_ONE = 0xC5 :: Opcode
op_TWO = 0xC6 :: Opcode
-- Others
op_HALT = 0xFF :: Opcode

-- Instructions

-- nullary Instructions
-- variable and stack manipulations
instr_POP = 0xE500000000000000 :: Instruction
-- constants
instr_NIL = 0xC000000000000000 :: Instruction
instr_TRUE = 0xC100000000000000 :: Instruction
instr_FALSE = 0xC200000000000000 :: Instruction
instr_MINUSONE = 0xC300000000000000 :: Instruction
instr_ZERO = 0xC400000000000000 :: Instruction
instr_ONE = 0xC500000000000000 :: Instruction
instr_TWO = 0xC600000000000000 :: Instruction
-- Others
instr_HALT = 0xFF00000000000000 :: Instruction

-------------------------------------------------
-- Instruction mnemonics
-------------------------------------------------
-- used for disassembly and printing instructions

mnemonicMap :: [(String, Opcode)]
mnemonicMap = [ -- variable and stack manipulations
                ("LVAR", op_LVAR),
                ("LSET", op_LSET),
                ("GVAR", op_GVAR),
                ("GSET", op_GSET),
                ("POP", op_POP),
                ("CONST", op_CONST),
                -- branching
                ("JUMP", op_JUMP),
                ("FJUMP", op_FJUMP),
                ("TJUMP", op_TJUMP),
                -- function call and return
                ("SAVE", op_SAVE),
                ("RETURN", op_RETURN),
                ("CALLJ", op_CALLJ),
                ("ARGS", op_ARGS),
                ("FN", op_FN),
                ("PRIM", op_PRIM),
                -- handling continuations
                ("SETCC", op_SETCC),
                ("CC", op_CC),
                -- unary operations
                ("CAR", op_CAR),
                ("CDR", op_CDR),
                ("CADR", op_CADR),
                ("NOT", op_NOT),
                ("LIST1", op_LIST1),
                ("COMPILER", op_COMPILER),
                ("DISPLAY", op_DISPLAY),
                ("WRITE", op_WRITE),
                ("RANDOM", op_RANDOM),
                -- binary operations
                ("ADD", op_ADD),
                ("SUB", op_SUB),
                ("MUL", op_MUL),
                ("DIV", op_DIV),
                ("REM", op_REM),
                ("LT", op_LT),
                ("GT", op_GT),
                ("LTE", op_LTE),
                ("GTE", op_GTE),
                ("NEQ", op_NEQ),
                ("EQL", op_EQL),
                ("CONS", op_CONS),
                ("LIST2", op_LIST2),
                ("NAME", op_NAME),
                ("EQ", op_EQ),
                ("EQUAL", op_EQUAL),
                -- ternary operations
                ("LIST3", op_LIST3),
                -- constants
                ("NIL", op_NIL),
                ("TRUE", op_TRUE),
                ("FALSE", op_FALSE),
                ("MINUSONE", op_MINUSONE),
                ("ZERO", op_ZERO),
                ("ONE", op_ONE),
                ("TWO", op_TWO),
                -- others
                ("HALT", op_HALT)]

mnemonic :: Opcode -> String
mnemonic op = 
    case (find (\ (m, o) -> o == op) mnemonicMap) of
      Just (mn, _) -> mn
      Nothing -> (show op)

-------------------------------------------------
-- VM definition
-------------------------------------------------

data Value = Null
           | True
           | False
           | Byte Word8
           | Int32 Data.Int.Int32
           | BigInt Integer
           | Character Char
           | Symbol String
           | Keyword String
           | Cons Value Value deriving (Show,Eq)

data ProgramCounter = PC Int deriving Show
data Frame = Frame [Value] deriving Show
data Environment = Env [Frame] deriving Show
data Stack = Stack [Value] deriving Show
data ArgumentCount = Nargs Int deriving Show
data HaltState = Running | Halted deriving Show

data VM = VM ProgramCounter Environment Stack ArgumentCount Code HaltState

instance Show VM
    where
    show (VM pc env stack nargs code haltstate) = "Bard VM: \n" ++
                                                  "  " ++ (show pc) ++ "\n" ++
                                                  "  " ++ (show env) ++ "\n" ++
                                                  "  " ++ (show stack) ++ "\n" ++
                                                  "  " ++ (show nargs) ++ "\n" ++
                                                  "  Code [" ++ (showCode code) ++ "]\n" ++
                                                  "  " ++ (show haltstate) ++ "\n"
showCode :: [Instruction] -> String
showCode instructions = intercalate ", " (map mnemonic (map opcode instructions))
    

-------------------------------------------------
-- Instruction implementations
-------------------------------------------------

pop :: [Value] -> [Value]
pop  (s:ss) = ss

executeInstruction :: Instruction -> VM -> VM
executeInstruction ins (VM  (PC pc) env (Stack s) nargs code halted) 
    -- constants
      | (op == op_NIL) = (VM  (PC (pc+1)) env (Stack (Null:s)) nargs code halted)
      | (op == op_TRUE) = (VM  (PC (pc+1)) env (Stack (Main.True:s)) nargs code halted)
      | (op == op_FALSE) = (VM  (PC (pc+1)) env (Stack (Main.False:s)) nargs code halted)
      | (op == op_MINUSONE) = (VM  (PC (pc+1)) env (Stack ((Int32 (-1)):s)) nargs code halted)
      | (op == op_ZERO) = (VM  (PC (pc+1)) env (Stack ((Int32 0):s)) nargs code halted)
      | (op == op_ONE) = (VM  (PC (pc+1)) env (Stack ((Int32 1):s)) nargs code halted)
      | (op == op_TWO) = (VM  (PC (pc+1)) env (Stack ((Int32 2):s)) nargs code halted)
      | (op == op_POP) = (VM  (PC (pc+1)) env (Stack (pop s)) nargs code halted)
      | (op == op_HALT) = (VM  (PC (pc+1)) env (Stack s) nargs code Halted)
      where op = (opcode ins)

-------------------------------------------------
-- Running the VM
-------------------------------------------------

getInstruction :: VM -> Instruction
getInstruction (VM (PC pc) env stack nargs code haltstate) = code !! pc

initVM :: [Instruction] -> VM
initVM code =
    VM (PC 0) (Env []) (Stack []) (Nargs 0) code Running

stepVM :: VM -> VM
stepVM (VM pc env stack nargs code Halted) = (VM pc env stack nargs code Halted)
stepVM vm = executeInstruction (getInstruction vm) vm

runVM :: VM -> VM
runVM (VM pc env stack nargs code Halted) = (VM pc env stack nargs code Halted)
runVM vm = runVM (stepVM vm)

-------------------------------------------------
-- main program
-------------------------------------------------

-- test constant instructions
testcodeC = [instr_NIL,
             instr_TRUE,
             instr_FALSE,
             instr_MINUSONE,
             instr_ZERO,
             instr_ONE,
             instr_TWO,
             instr_HALT]

-- test nullary operations
testcode0 = [instr_ONE,
             instr_TWO,
             instr_POP,
             instr_HALT]

-- test unary operations


main = do
       args <- getArgs
       let bardprog = args !! 0
       let prog = case bardprog of
                    "c" -> testcodeC
                    "0" -> testcode0
                    _ -> testcodeC
       let vm = initVM prog
       putStrLn "Bard VM v 1.0"
       putStrLn (show vm)
       putStrLn ""
       putStrLn "Running..."
       let vm' = runVM vm
       putStrLn ""
       putStrLn (show vm')