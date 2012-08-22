/*
 * Bard
 * bytecode definitions
 *
 */

typedef unsigned long long vmword;

#define _HALT   (vmword)0ULL
#define _VAL    (vmword)1ULL
#define _LREF   (vmword)2ULL
#define _LSETR  (vmword)3ULL
#define _MREF   (vmword)4ULL
#define _MSETR  (vmword)5ULL
#define _SREF   (vmword)6ULL
#define _SSETR  (vmword)7ULL
#define _DEF    (vmword)8ULL
#define _CLOSE  (vmword)9ULL
#define _PRIM  (vmword)10ULL
#define _MODL  (vmword)11ULL
#define _INMOD (vmword)12ULL
#define _JUMP  (vmword)13ULL
#define _FJUMP (vmword)14ULL
#define _TJUMP (vmword)15ULL
#define _SAVE  (vmword)16ULL
#define _APPLY (vmword)17ULL
#define _RET   (vmword)18ULL


typedef struct vmstruct {
  vmword  pc;
  vmword  instr;
  vmword* code;
  vmword* fn;
  vmword  nvals;
  vmword* vals;
  vmword  stackptr;
  vmword* stack;
  vmword* env;
  vmword* module;
  vmword* modules;
} vm;


void exec (vm* vm);
