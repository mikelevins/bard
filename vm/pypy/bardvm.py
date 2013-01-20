"""
Bard VM in Pypy
"""

# opcodes

HALT   =0
CONST  =1
LREF   =2
LSET   =3
GREF   =4
GSET   =5
POPV   =6
POPC   =7
PRIM   =8
JUMP   =9
FJUMP =10
TJUMP =11
SAVE  =12
CALL  =13
RETURN=14
CC    =15
SETCC =16

# constants

GFalse = False
GTrue = True
GNothing = None

# registers

Halted = False
Fn = None
Instr = None
Code = None
Pc = 0
NVals = 0
Env = {}
Globals = {}
Vals = []
Stack = []


def env_ref(env,var):
    return env[var]

def env_set(env,var,val):
    env[var]=val

def global_ref(globs,var):
    return globs[var]

def global_set(globs,var,val):
    globs[var]=val

def is_false(x):
    return (x == GFalse) or (x == GNone)

def is_true(x):
    if (is_false(s)):
        return False
    return True

def _halt():
    Halted = True

def _pushv(v):
    NVals+=1
    Vals.append(v)

def _popv(v):
    NVals-=1
    return Vals.pop()

def _pushc(c):
    Stack.append(v)

def _popc(v):
    return Stack.pop()

def _prim(p):
    argcount=check_prim_argcount(p,NVals)
    args = _popnv(argcount)
    _pushnv(apply_prim(p,args))

def _jump(d):
    Pc=d

def _fjump(d):
    if (is_false(_popv())):
        Pc=d

def _tjump(d):
    if (is_true(_popv())):
        Pc=d

def cc():
    _pushv(_makecc())

def setcc():
    restore_cc(_popv())
    
def vmfetch():
    Instr=code_ref(Code,Pc)

def vmexec():
    apply_op(Instr.op,Instr.args)



