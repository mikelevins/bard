#ifndef CELL_H
#define CELL_H

#include <inttypes.h>

typedef struct cell {
  uint32_t car;
  uint32_t cdr;
} cell;

struct cell* make_cell();

#endif
