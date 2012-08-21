/*
 * Bard
 * vm main loop
 *
 */

#include <stdio.h>
#include "bytecode.h"
#include "vm.h"

void exec (char* start) {
  void* jumptable[19]={&&halt,&&val,&&lref,&&lsetr,
                     &&mref,&&msetr,&&sref,&&ssetr,
                     &&def,&&close,&&prim,&&modl,
                     &&inmod,&&jump,&&fjump,&&tjump,
                     &&save,&&apply,&&ret};
  goto *(jumptable[*start]);
  do {
  halt: 
    printf("\nhalt");
    break;
  val: 
    printf("\nval");
    start+=1;
    goto *(jumptable[*start]);
    break;
  lref: 
    printf("\nlref");
    start+=1;
    goto *(jumptable[*start]);
    break;
  lsetr: 
    printf("\nlsetr");
    start+=1;
    goto *(jumptable[*start]);
    break;
  mref: 
    printf("\nmref");
    start+=1;
    goto *(jumptable[*start]);
    break;
  msetr: 
    printf("\nmsetr");
    start+=1;
    goto *(jumptable[*start]);
    break;
  sref: 
    printf("\nsref");
    start+=1;
    goto *(jumptable[*start]);
    break;
  ssetr: 
    printf("\nssetr");
    start+=1;
    goto *(jumptable[*start]);
    break;
  def: 
    printf("\ndef");
    start+=1;
    goto *(jumptable[*start]);
    break;
  close: 
    printf("\nclose");
    start+=1;
    goto *(jumptable[*start]);
    break;
  prim: 
    printf("\nprim");
    start+=1;
    goto *(jumptable[*start]);
    break;
  modl: 
    printf("\nmodl");
    start+=1;
    goto *(jumptable[*start]);
    break;
  inmod: 
    printf("\ninmod");
    start+=1;
    goto *(jumptable[*start]);
    break;
  jump: 
    printf("\njump");
    start+=1;
    goto *(jumptable[*start]);
    break;
  fjump: 
    printf("\nfjump");
    start+=1;
    goto *(jumptable[*start]);
    break;
  tjump: 
    printf("\ntjump");
    start+=1;
    goto *(jumptable[*start]);
    break;
  save: 
    printf("\nsave");
    start+=1;
    goto *(jumptable[*start]);
    break;
  apply: 
    printf("\napply");
    start+=1;
    goto *(jumptable[*start]);
    break;
  ret:    
    printf("\nret");
    start+=1;
    goto *(jumptable[*start]);
    break;

  } while(1);

}
