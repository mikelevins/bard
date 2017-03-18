#include <stdlib.h>
#include <stdio.h>
#include <time.h>


// ======================================================================
// ABOUT
// ======================================================================
// compiled with:
// gcc -O2 -o bard bard.c
// output bardvm is 16K in size
//
// test program:
//
// LREF  0 0
// CONST 100000
// GT
// FJUMP 9
// LREF  0 0
// CONST 1
// ADD
// LSET  0 0
// JUMP  0
// HALT
//
// execution time: 24.6 milliseconds
// 406,504,065 VM instructions per second
// test environment:
// OSX Darwin 11.4.2 Darwin Kernel Version 11.4.2 (Lion 10.7.5)
// iMac, 27-inch, mid 2011
// processor 3.4 GHz Intel Core i7
// memory 16GB 1333MHz DDR3
// gcc (GCC) 4.7.1

// ======================================================================
// Main Program
// ======================================================================

#define tINSTR0 0
#define tINSTR1 1
#define tINSTR2 2

#define HALT 0
#define CONST 1
#define LREF 2
#define LSET 3
#define JUMP 4
#define FJUMP 5
#define GT 6
#define ADD 7


// registers
static int r_HALTED=0;
static void* r_CODE[10];
static void* r_INSTR=0L;
static int r_PC=0;
static int r_NVALS=0;
static int r_VALS[8];
static int r_ENV[1][1];

static long last_val=0;

struct instruction0 {
  int tag;
  int op;
};

struct instruction0* instr0(int opcode){
  struct instruction0* i = (struct instruction0*)malloc(sizeof(struct instruction0));
  i->tag=tINSTR0;
  i->op=opcode;
  return i;
}

struct instruction1 {
  int tag;
  int op;
  int arg;
};

struct instruction1* instr1(int opcode, int arg){
  struct instruction1* i = (struct instruction1*)malloc(sizeof(struct instruction1));
  i->tag=tINSTR1;
  i->op=opcode;
  i->arg=arg;
  return i;
}

struct instruction2 {
  int tag;
  int op;
  int arg1;
  int arg2;
};

struct instruction2* instr2(int opcode, int arg1, int arg2){
  struct instruction2* i = (struct instruction2*)malloc(sizeof(struct instruction2));
  i->tag=tINSTR2;
  i->op=opcode;
  i->arg1=arg1;
  i->arg2=arg2;
  return i;
}

void pushv(int v){
  r_VALS[r_NVALS]=v;
  r_NVALS++;
}

int popv(){
  r_NVALS--;
  return r_VALS[r_NVALS];
}

void op_halt(){
  r_HALTED=1;
}

void op_const(struct instruction1* i1){
  int k = i1->arg;
  pushv(k);
  r_PC++;
}

void op_lref(struct instruction2* i2){
  int i = i2->arg1;
  int j = i2->arg2;
  int v = r_ENV[i][j];
  pushv(v);
  r_PC++;
}

void op_lset(struct instruction2* i2){
  int i = i2->arg1;
  int j = i2->arg2;
  int v = popv();
  r_ENV[i][j]=v;
  r_PC++;
}

void op_jump(struct instruction1* i1){
  int d = i1->arg;
  r_PC=d;
}

void op_fjump(struct instruction1* i1){
  int d = i1->arg;
  int v = popv();
  if (v==0) {
    r_PC=d;
  } else {
    r_PC++;
  }
}

void op_gt(struct instruction0* i0){
  int a = popv();
  int b = popv();
  if (a>b){
    pushv(1);
  }else{
    pushv(0);
  }
  r_PC++;
}

void op_add(struct instruction0* i0){
  int a = popv();
  int b = popv();
  last_val=(a+b);
  pushv(last_val);
  r_PC++;
}

void vmexec() {
  struct instruction0* i0 = (struct instruction0*)r_INSTR;
  struct instruction1* i1 = (struct instruction1*)r_INSTR;
  struct instruction2* i2 = (struct instruction2*)r_INSTR;
  int op = i0->op;
  switch (op){
  case HALT:
    op_halt();
    break;
  case CONST:
    op_const(i1);
    break;
  case LREF:
    op_lref(i2);
    break;
  case LSET:
    op_lset(i2);
    break;
  case JUMP:
    op_jump(i1);
    break;
  case FJUMP:
    op_fjump(i1);
    break;
  case GT:
    op_gt(i0);
    break;
  case ADD:
    op_add(i0);
    break;
  default:
    break;
  }
}

void fetch() {
  r_INSTR = r_CODE[r_PC];
}
  
int main (int argc, char*argv[]) {

  // timing
  clock_t start_time, end_time, elapsed;
  double diff;

  // test code
  
  r_CODE[0]=(void*)instr2(LREF,0,0);
  r_CODE[1]=(void*)instr1(CONST,1000000);
  r_CODE[2]=(void*)instr0(GT);
  r_CODE[3]=(void*)instr1(FJUMP,9);
  r_CODE[4]=(void*)instr2(LREF,0,0);
  r_CODE[5]=(void*)instr1(CONST,1);
  r_CODE[6]=(void*)instr0(ADD);
  r_CODE[7]=(void*)instr2(LSET,0,0);
  r_CODE[8]=(void*)instr1(JUMP,0);
  r_CODE[9]=(void*)instr0(HALT);



  // ----------------------------------------------------------------------
  // BEGIN VM
  // ----------------------------------------------------------------------

  start_time = clock();

  printf("\n\n========================================================================");
  printf("\nBard 0.4 VM test");
  printf("\n========================================================================\n");

  printf("\nStarting execution...\n");

  while(!r_HALTED){
    fetch();
    vmexec();
  }
    
  // ----------------------------------------------------------------------
  // END VM
  // ----------------------------------------------------------------------
    
  // shut down, report statistics
  end_time = clock();
  elapsed = end_time - start_time;
  diff = (double)elapsed;

  printf("\nEnding value = %ld\n", last_val);
  printf("\nRan to completion in %ld microseconds\n", (elapsed));
  printf(" (%f seconds)\n", (diff/CLOCKS_PER_SEC));
  printf("\n========================================================================\n\n");
}
