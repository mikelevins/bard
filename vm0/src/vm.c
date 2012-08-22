/*
 * Bard
 * vm main loop
 *
 */

#import <stdio.h>
#import "vm.h"

void exec (vm* vm) {
  vmword* program = vm->code;
  void* jumptable[19]={&&halt,&&val,&&lref,&&lsetr,
                       &&mref,&&msetr,&&sref,&&ssetr,
                       &&def,&&close,&&prim,&&modl,
                       &&inmod,&&jump,&&fjump,&&tjump,
                       &&save,&&apply,&&ret};
  goto *(jumptable[*program]);
  do {
  halt: 
    break;
  val: 
    printf("\nval");
    program+=1;
    goto *(jumptable[*program]);
    break;
  lref: 
    printf("\nlref");
    program+=1;
    goto *(jumptable[*program]);
    break;
  lsetr: 
    printf("\nlsetr");
    program+=1;
    goto *(jumptable[*program]);
    break;
  mref: 
    printf("\nmref");
    program+=1;
    goto *(jumptable[*program]);
    break;
  msetr: 
    printf("\nmsetr");
    program+=1;
    goto *(jumptable[*program]);
    break;
  sref: 
    printf("\nsref");
    program+=1;
    goto *(jumptable[*program]);
    break;
  ssetr: 
    printf("\nssetr");
    program+=1;
    goto *(jumptable[*program]);
    break;
  def: 
    printf("\ndef");
    program+=1;
    goto *(jumptable[*program]);
    break;
  close: 
    printf("\nclose");
    program+=1;
    goto *(jumptable[*program]);
    break;
  prim: 
    printf("\nprim");
    program+=1;
    goto *(jumptable[*program]);
    break;
  modl: 
    printf("\nmodl");
    program+=1;
    goto *(jumptable[*program]);
    break;
  inmod: 
    printf("\ninmod");
    program+=1;
    goto *(jumptable[*program]);
    break;
  jump: 
    printf("\njump");
    program+=1;
    goto *(jumptable[*program]);
    break;
  fjump: 
    printf("\nfjump");
    program+=1;
    goto *(jumptable[*program]);
    break;
  tjump: 
    printf("\ntjump");
    program+=1;
    goto *(jumptable[*program]);
    break;
  save: 
    printf("\nsave");
    program+=1;
    goto *(jumptable[*program]);
    break;
  apply: 
    printf("\napply");
    program+=1;
    goto *(jumptable[*program]);
    break;
  ret:    
    printf("\nret");
    program+=1;
    goto *(jumptable[*program]);
    break;

  } while(1);

}
