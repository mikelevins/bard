/*
 * Bard
 * bytecode definitions
 *
 */

typedef struct vmstruct {
  unsigned long long  pc;
  unsigned long long  instr;
  unsigned long long* code;
  unsigned long long* fn;
  unsigned long long  nvals;
  unsigned long long* vals;
  unsigned long long  stackptr;
  unsigned long long* stack;
  unsigned long long* env;
  unsigned long long* module;
  unsigned long long* modules;
} vm;



void exec (char* start);
