/*
 * Bard
 * vm main loop
 *
 */

#include <stdio.h>
#include "bytecode.h"
#include "vm.h"

int main () {
  printf("\nbard vm 0.1\n\n");

  printf("\ntest1: _VAL _VAL _DEF,_HALT:\n");
  char test1[] = {_VAL,_VAL,_DEF,_HALT};
  exec(test1);

  printf("\n\ntest2: _VAL,_FJUMP,_PRIM,_SAVE,_APPLY,_RET,_HALT :\n");
  char test2[] = {_VAL,_FJUMP,_PRIM,_SAVE,_APPLY,_RET,_HALT};
  exec(test2);

  printf("\n\ndone.\n");
}
