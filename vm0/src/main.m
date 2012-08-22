/*
 * Bard
 * vm main loop
 *
 */

#import <stdio.h>
#import "vm.h"

int main () {
  printf("\nbard vm 0.1\n\n");

  printf("\ntest1: _VAL _VAL _DEF,_HALT:\n");
  vmword prog1[] = {_VAL,_VAL,_DEF,_HALT};
  vm test1;
  test1.code=prog1;
  exec(&test1);

  printf("\n\ntest2: _VAL,_FJUMP,_PRIM,_SAVE,_APPLY,_RET,_HALT :\n");
  vmword prog2[] = {_VAL,_FJUMP,_PRIM,_SAVE,_APPLY,_RET,_HALT};
  vm test2;
  test2.code=prog2;
  exec(&test2);

  printf("\n\ndone.\n");
}
