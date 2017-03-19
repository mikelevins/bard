#include <stdlib.h>
#include "cell.h"

struct cell* make_cell() {
  struct cell* val = malloc (sizeof (struct cell));
  if (val == NULL) return NULL;
  val->car = 0;
  val->cdr = 0;
  return val;
}
