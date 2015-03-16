
# opcode definitions
# ------------------
const HALT  =  0
const GREF  =  1
const GSET  =  2
const LREF  =  3
const LSET  =  4
const GO    =  5
const TGO   =  6
const FGO   =  7
const SAVE  =  8
const RET   =  9
const CALL  = 10
const ARGS  = 11
const ARGSN = 12
const METH  = 13
const PRIM  = 14
const SETCC = 15
const CC    = 16

# type definitions
# ------------------

type Val = ref object of RootObj
type IntVal = ref object of Val  
  value: int
type FloatVal = ref object of Val  
  value: float
type StringVal = ref object of Val  
  value: string
type SymbolVal = ref object of Val  
  name: string

type Instruction = object
  opcode: int
  args: seq[Val]

type Code = seq[Instruction]
  
type
  Program = object
    code: Code

# vm registers
# ------------------

var
  program: Program 
  code: Code = @[]
  pc: int = 0
  running: bool = true
  instruction: Instruction

# vm procedures
# ------------------
    
proc vmexec(instruction:Instruction) =
  case instruction.opcode:
    of HALT:
      running=false
      pc += 1
    of GREF:
      running=false
      pc += 1
    of GSET:
      running=false
      pc += 1
    of LREF:
      running=false
      pc += 1
    of LSET:
      running=false
      pc += 1
    of GO:
      running=false
      pc += 1
    of TGO:
      running=false
      pc += 1
    of FGO:
      running=false
      pc += 1
    of SAVE:
      running=false
      pc += 1
    of RET:
      running=false
      pc += 1
    of CALL:
      running=false
      pc += 1
    of ARGS:
      running=false
      pc += 1
    of ARGSN:
      running=false
      pc += 1
    of METH:
      running=false
      pc += 1
    of PRIM:
      running=false
      pc += 1
    of SETCC:
      running=false
      pc += 1
    of CC:
      running=false
      pc += 1
    else: echo "Invalid opcode"    

proc runvm(prog: Program) =
  program = prog
  code = program.code
  pc = 0
  while running:
    instruction = code[pc]
    vmexec(instruction)
  
# main program
# ------------------
    
proc main(): void =
  echo ("Bard VM 4.0")
  let i = Instruction(opcode:HALT)
  let c = @[i,i,i]
  let p = Program(code:c)
  runvm(p)

main()
